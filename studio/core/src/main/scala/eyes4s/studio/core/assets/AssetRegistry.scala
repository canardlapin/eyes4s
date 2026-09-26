/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eyes4s.studio.core.assets

import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, CanonicalDigest, CodecError, VersionedCodec}
import eyes4s.studio.core.backend.{DatasetRevision, TrialKey}
import eyes4s.studio.core.document.{
  CanonicalJson,
  DatasetRevisionSpec,
  ImagePlacement,
  ScreenSize,
  StudioSchemaIds
}
import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/** Why an asset registry, or one of its values, was refused. Every case
  * names what it was applied to; `message` is a default English rendering.
  */
enum AssetError derives CanEqual:
  case BadFileName(value: String, reason: String)
  case BlankItem(trial: TrialKey)

  /** A display kind token that is none of [[DisplayKind.token]]. */
  case UnrecognisedKind(trial: TrialKey, token: String)

  /** An image display that names no image file. */
  case AssetRequired(trial: TrialKey, kind: DisplayKind)

  /** A blank display (with or without the cross) that names an asset. */
  case AssetNotShown(trial: TrialKey, kind: DisplayKind, file: AssetFile)
  case PlacementOffScreen(trial: TrialKey, placement: ImagePlacement, screen: ScreenSize)
  case DuplicateTrial(trial: TrialKey)

  /** One file name bound to more than one byte digest. */
  case ConflictingDigests(file: AssetFile, digests: Vector[ByteDigest])

  /** One file name present for some trials and missing for others. */
  case PresentAndMissing(file: AssetFile, present: TrialKey, missing: TrialKey)

  /** The same stored file name listed twice. */
  case DuplicateAsset(file: AssetFile)

  /** Repair named a file no trial is missing. */
  case NotMissing(file: AssetFile, missing: Vector[AssetFile])
  case NoTrialInventory(dataset: DatasetRevision)
  case WrongDataset(registry: DatasetRevision, dataset: DatasetRevision)
  case WrongInventory(dataset: DatasetRevision, registry: ByteDigest, source: ByteDigest)

  def message: String = this match
    case BadFileName(value, reason)     => s"Asset file name '$value' is refused: $reason."
    case BlankItem(trial)               => s"Trial ${trial.label} names a blank match item."
    case UnrecognisedKind(trial, token) =>
      s"Trial ${trial.label} has display kind '$token', which is not one of " +
        DisplayKind.values.map(_.token).mkString(", ") + "."
    case AssetRequired(trial, kind) =>
      s"Trial ${trial.label} shows ${kind.token} but names no image file."
    case AssetNotShown(trial, kind, file) =>
      s"Trial ${trial.label} shows ${kind.token}, which displays no asset, but names ${file.value}."
    case PlacementOffScreen(trial, placement, screen) =>
      s"Trial ${trial.label}'s image frame ${placement.render} is not inside the ${screen.render} screen."
    case DuplicateTrial(trial)             => s"Trial ${trial.label} is listed more than once."
    case ConflictingDigests(file, digests) =>
      s"Asset ${file.value} has ${digests.size} different digests: " +
        digests.map(_.hex.take(12)).mkString(", ") + "."
    case PresentAndMissing(file, present, missing) =>
      s"Asset ${file.value} is present for ${present.label} and missing for ${missing.label}."
    case DuplicateAsset(file)     => s"Stored asset ${file.value} is listed more than once."
    case NotMissing(file, absent) =>
      s"No trial is missing ${file.value}; missing: " +
        (if absent.isEmpty then "none" else absent.map(_.value).mkString(", ")) + "."
    case NoTrialInventory(dataset) =>
      s"Dataset ${dataset.label} has no trial inventory, so it states no displays."
    case WrongDataset(registry, dataset) =>
      s"The asset registry belongs to dataset ${registry.label}, not ${dataset.label}."
    case WrongInventory(dataset, registry, source) =>
      s"The asset registry was built from trial inventory ${registry.hex.take(12)}, but " +
        s"dataset ${dataset.label}'s is ${source.hex.take(12)}."

// ---------------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------------

/** What a trial showed on the screen (DESIGN_SPEC section 9). "Missing
  * asset" is not a kind: it is the state of a display whose asset is
  * named but has no bytes ([[AssetLink.Missing]], [[DisplayState.MissingAsset]]).
  */
enum DisplayKind derives CanEqual, Codec.AsObject:
  case Image, Blank, BlankWithFixationCross, Cue, Unknown

  /** The spelling trial inventories use (fixtures/studio-golden trials.csv). */
  def token: String = this match
    case Image                  => "image"
    case Blank                  => "blank"
    case BlankWithFixationCross => "blank+fixation-cross"
    case Cue                    => "cue"
    case Unknown                => "unknown"

object DisplayKind:
  /** Parse an inventory token; an empty cell is `Unknown` (the inventory did
    * not say), an unrecognised one is refused.
    */
  def parse(trial: TrialKey, token: String): Either[AssetError, DisplayKind] =
    val t = token.trim
    if t.isEmpty then Right(Unknown)
    else values.find(_.token == t).toRight(AssetError.UnrecognisedKind(trial, token))

/** An asset's file name as the trial inventory names it: one path segment. */
final case class AssetFile private (value: String) derives CanEqual

object AssetFile:
  def of(value: String): Either[AssetError, AssetFile] =
    if value.trim.isEmpty then Left(AssetError.BadFileName(value, "it is blank"))
    else if value.exists(c => c == '/' || c == '\\') then
      Left(AssetError.BadFileName(value, "it is not a single path segment"))
    else if value == "." || value == ".." then
      Left(AssetError.BadFileName(value, "it names a directory"))
    else Right(new AssetFile(value))

  given Ordering[AssetFile] = Ordering.by(_.value)
  given Codec[AssetFile]    = AssetCodecs.validated(of, _.value)

/** A stored asset: its file name and the SHA-256 of its exact bytes, the
  * digest under which a bundle stores it (`inputs/<sha256>/<file>`).
  */
final case class AssetRef(file: AssetFile, sha256: ByteDigest) derives CanEqual

object AssetRef:
  given Encoder.AsObject[AssetRef] =
    Encoder.forProduct2("file", "sha256")(a => (a.file, a.sha256.hex))
  given Decoder[AssetRef] =
    Decoder
      .forProduct2[(AssetFile, String), AssetFile, String]("file", "sha256")((_, _))
      .emap((f, hex) => ByteDigest.parse(hex).bimap(_.message, AssetRef(f, _)))

/** A display's asset: stored (`Present`) or named by the inventory with no
  * bytes to show (`Missing`). Missing is a state of its own, never a blank.
  */
enum AssetLink derives CanEqual:
  case Present(asset: AssetRef)
  case Missing(file: AssetFile)

  def fileName: AssetFile = this match
    case Present(a) => a.file
    case Missing(f) => f

object AssetLink:
  given Encoder[AssetLink] = Encoder.instance {
    case Present(a) => Json.obj("Present" -> a.asJson)
    case Missing(f) => Json.obj("Missing" -> Json.obj("file" -> f.asJson))
  }
  given Decoder[AssetLink] = Decoder.instance { c =>
    c.keys.map(_.toVector) match
      case Some(Vector("Present")) => c.get[AssetRef]("Present").map(Present(_))
      case Some(Vector("Missing")) =>
        c.downField("Missing").get[AssetFile]("file").map(Missing(_))
      case other =>
        Left(DecodingFailure(s"expected Present or Missing, got $other", c.history))
  }

/** The item a trial is matched on. It is an attribute of the trial, never of
  * its display: imagery trials that share one blank display keep their own
  * items.
  */
final case class MatchItem private (value: String) derives CanEqual

object MatchItem:
  def of(trial: TrialKey, value: String): Either[AssetError, MatchItem] =
    Either.cond(value.trim.nonEmpty, new MatchItem(value), AssetError.BlankItem(trial))

  given Encoder[MatchItem] = Encoder[String].contramap(_.value)
  given Decoder[MatchItem] = Decoder[String].emap(v =>
    Either.cond(v.trim.nonEmpty, new MatchItem(v), "a match item is blank")
  )

/** What a trial showed, with its asset: an image always names one, a blank
  * never does, a cue or an unknown display may. A value of this type is a
  * valid display; the kind and asset are derived from it.
  */
enum Display derives CanEqual:
  case Image(asset: AssetLink)
  case Blank
  case BlankWithFixationCross
  case Cue(asset: Option[AssetLink])
  case Unknown(asset: Option[AssetLink])

  def kind: DisplayKind = this match
    case Image(_)               => DisplayKind.Image
    case Blank                  => DisplayKind.Blank
    case BlankWithFixationCross => DisplayKind.BlankWithFixationCross
    case Cue(_)                 => DisplayKind.Cue
    case Unknown(_)             => DisplayKind.Unknown

  def link: Option[AssetLink] = this match
    case Image(a)                       => Some(a)
    case Cue(a)                         => a
    case Unknown(a)                     => a
    case Blank | BlankWithFixationCross => None

  /** What a renderer shows: a missing asset is its own state, never blank. */
  def state: DisplayState =
    def stored(a: Option[AssetLink]) = a.collect { case AssetLink.Present(r) => r }
    this match
      case Image(AssetLink.Present(a))         => DisplayState.Image(a)
      case Image(AssetLink.Missing(f))         => DisplayState.MissingAsset(kind, f)
      case Cue(Some(AssetLink.Missing(f)))     => DisplayState.MissingAsset(kind, f)
      case Unknown(Some(AssetLink.Missing(f))) => DisplayState.MissingAsset(kind, f)
      case Cue(a)                              => DisplayState.Cue(stored(a))
      case Unknown(a)                          => DisplayState.Unknown(stored(a))
      case Blank                               => DisplayState.Blank
      case BlankWithFixationCross              => DisplayState.BlankWithFixationCross

object Display:
  /** A display of `kind` with `asset`: refused when an image names none or a
    * blank names one.
    */
  def of(
      trial: TrialKey,
      kind: DisplayKind,
      asset: Option[AssetLink]
  ): Either[AssetError, Display] =
    (kind, asset) match
      case (DisplayKind.Image, Some(a)) => Right(Image(a))
      case (DisplayKind.Image, None)    => Left(AssetError.AssetRequired(trial, kind))
      case (DisplayKind.Blank | DisplayKind.BlankWithFixationCross, Some(a)) =>
        Left(AssetError.AssetNotShown(trial, kind, a.fileName))
      case (DisplayKind.Blank, None)                  => Right(Blank)
      case (DisplayKind.BlankWithFixationCross, None) => Right(BlankWithFixationCross)
      case (DisplayKind.Cue, a)                       => Right(Cue(a))
      case (DisplayKind.Unknown, a)                   => Right(Unknown(a))

  given Encoder[Display] = Encoder.instance { d =>
    val fields = d match
      case Image(a)                       => Json.obj("asset" -> a.asJson)
      case Cue(a)                         => Json.obj("asset" -> a.asJson)
      case Unknown(a)                     => Json.obj("asset" -> a.asJson)
      case Blank | BlankWithFixationCross => Json.obj()
    Json.obj(d.kind.productPrefix -> fields)
  }

  /** One member named by the kind. An image requires `asset` and a blank
    * refuses one, so a missing asset can never read back as a blank.
    */
  given Decoder[Display] = Decoder.instance { c =>
    def fail[A](message: String): Decoder.Result[A] =
      Left(DecodingFailure(message, c.history))
    c.keys.map(_.toVector) match
      case Some(Vector(name)) =>
        val body     = c.downField(name)
        val members  = body.keys.map(_.toVector).getOrElse(Vector.empty)
        def optional = body.getOrElse[Option[AssetLink]]("asset")(None)
        DisplayKind.values.find(_.productPrefix == name) match
          case Some(DisplayKind.Image)     => body.get[AssetLink]("asset").map(Image(_))
          case Some(DisplayKind.Cue)       => optional.map(Cue(_))
          case Some(DisplayKind.Unknown)   => optional.map(Unknown(_))
          case Some(_) if members.nonEmpty =>
            fail(s"$name displays no asset but has ${members.mkString(", ")}")
          case Some(DisplayKind.Blank)                  => Right(Blank)
          case Some(DisplayKind.BlankWithFixationCross) => Right(BlankWithFixationCross)
          case None                                     => fail(s"unknown display $name")
      case other => fail(s"expected one display kind, got $other")
  }

/** What a renderer shows for a trial: its kind, with a missing asset as its
  * own state (hatched), distinct from every blank.
  */
enum DisplayState derives CanEqual:
  case Image(asset: AssetRef)
  case Blank
  case BlankWithFixationCross
  case Cue(asset: Option[AssetRef])
  case Unknown(asset: Option[AssetRef])
  case MissingAsset(kind: DisplayKind, file: AssetFile)

/** One trial's display (its kind and asset, by byte digest or missing),
  * where the image frame lies in the screen frame, and the item it is
  * matched on, which belongs to the trial and not to what it showed.
  */
final case class TrialDisplay(
    trial: TrialKey,
    item: Option[MatchItem],
    display: Display,
    placement: ImagePlacement
) derives CanEqual:
  def kind: DisplayKind        = display.kind
  def asset: Option[AssetLink] = display.link
  def state: DisplayState      = display.state

  def isMissing: Boolean = asset.exists {
    case AssetLink.Missing(_) => true
    case _                    => false
  }

object TrialDisplay:
  given Encoder.AsObject[TrialDisplay] =
    Encoder.forProduct4("trial", "item", "display", "placement")(d =>
      (d.trial, d.item, d.display, d.placement)
    )
  given Decoder[TrialDisplay] =
    Decoder.forProduct4("trial", "item", "display", "placement")(TrialDisplay.apply)

/** One trial of a trial inventory's display columns, as text: the item, the
  * display kind token and the image file; an empty cell states nothing.
  */
final case class DisplayRow(trial: TrialKey, item: String, kind: String, file: String)
    derives CanEqual

/** A missing asset and the trials that name it. */
final case class MissingAsset(file: AssetFile, trials: Vector[TrialKey]) derives CanEqual

/** The registry's counts: distinct files the displays name, how many are
  * stored, and the missing ones.
  */
final case class AssetSummary(files: Int, present: Int, missing: Vector[MissingAsset])
    derives CanEqual

// ---------------------------------------------------------------------------
// The registry
// ---------------------------------------------------------------------------

/** Every trial's display for one dataset revision (ticket S2.10): the trial
  * inventory it was read from (by byte digest), the screen, and one
  * [[TrialDisplay]] per trial, in inventory order.
  *
  * It lies next to the document's dataset revision rather than inside it: it
  * is keyed by the revision and its trial inventory's digest, and
  * [[AssetRegistry.check]] ties it to a [[DatasetRevisionSpec]].
  *
  * Built only through [[AssetRegistry.of]]: trials are distinct, every image
  * frame lies inside the screen, one file name has one digest, and a file is
  * never present for one trial and missing for another.
  */
final case class AssetRegistry private (
    dataset: DatasetRevision,
    inventory: ByteDigest,
    screen: ScreenSize,
    trials: Vector[TrialDisplay]
) derives CanEqual:
  private lazy val byTrial: Map[TrialKey, TrialDisplay] = trials.map(d => d.trial -> d).toMap

  def display(trial: TrialKey): Option[TrialDisplay] = byTrial.get(trial)

  /** The item `trial` is matched on: its own attribute, whatever it showed. */
  def matchItem(trial: TrialKey): Option[MatchItem] = display(trial).flatMap(_.item)

  /** Every stored asset the displays name, by file name. */
  def assets: Vector[AssetRef] =
    trials
      .flatMap(_.asset)
      .collect { case AssetLink.Present(a) => a }
      .distinct
      .sortBy(_.file)

  /** Every named file without bytes, with the trials naming it. */
  def missing: Vector[MissingAsset] =
    trials
      .flatMap(d => d.asset.collect { case AssetLink.Missing(f) => f -> d.trial })
      .groupMap(_._1)(_._2)
      .toVector
      .map(MissingAsset(_, _))
      .sortBy(_.file)

  def summary: AssetSummary =
    val present = assets
    val absent  = missing
    AssetSummary(present.size + absent.size, present.size, absent)

  def count(kind: DisplayKind): Int = trials.count(_.kind == kind)

  /** Resolve every display missing `file` to the stored `asset` (Repair). */
  def repair(file: AssetFile, asset: AssetRef): Either[AssetError, AssetRegistry] =
    val absent = missing.map(_.file)
    if !absent.contains(file) then Left(AssetError.NotMissing(file, absent))
    else
      AssetRegistry.of(
        dataset,
        inventory,
        screen,
        trials.map(d =>
          d.copy(display = d.display match
            case Display.Image(AssetLink.Missing(`file`)) =>
              Display.Image(AssetLink.Present(asset))
            case Display.Cue(Some(AssetLink.Missing(`file`))) =>
              Display.Cue(Some(AssetLink.Present(asset)))
            case Display.Unknown(Some(AssetLink.Missing(`file`))) =>
              Display.Unknown(Some(AssetLink.Present(asset)))
            case other => other)
        )
      )

object AssetRegistry:
  def of(
      dataset: DatasetRevision,
      inventory: ByteDigest,
      screen: ScreenSize,
      trials: Vector[TrialDisplay]
  ): Either[AssetError, AssetRegistry] =
    val links = trials.flatMap(d => d.asset.map(_ -> d.trial))
    for
      _ <- firstRepeat(trials.map(_.trial)).map(AssetError.DuplicateTrial(_)).toLeft(())
      _ <- trials.traverse_ { d =>
        val p = d.placement
        Either.cond(
          p.left >= 0 && p.top >= 0 && p.left.toLong + p.width <= screen.width &&
            p.top.toLong + p.height <= screen.height,
          (),
          AssetError.PlacementOffScreen(d.trial, p, screen)
        )
      }
      _ <- links
        .collect { case (AssetLink.Present(a), _) => a }
        .groupMap(_.file)(_.sha256)
        .toVector
        .sortBy(_._1)
        .traverse_ { (file, digests) =>
          val distinct = digests.distinct
          Either.cond(distinct.size == 1, (), AssetError.ConflictingDigests(file, distinct))
        }
      _ <- links
        .collectFirst { case (AssetLink.Missing(f), missing) =>
          links.collectFirst {
            case (AssetLink.Present(a), present) if a.file == f =>
              AssetError.PresentAndMissing(f, present, missing)
          }
        }
        .flatten
        .toLeft(())
    yield new AssetRegistry(dataset, inventory, screen, trials)

  private def firstRepeat[A](values: Vector[A]): Option[A] =
    values.diff(values.distinct).headOption

  /** Build the registry of `dataset` from its trial inventory's display
    * columns and the assets stored for it: an image file among `stored` is
    * `Present`, any other named file `Missing`. Every trial's image frame is
    * the dataset's image placement.
    */
  def fromRows(
      dataset: DatasetRevisionSpec,
      rows: Vector[DisplayRow],
      stored: Vector[AssetRef]
  ): Either[AssetError, AssetRegistry] =
    for
      inventory <- dataset.sources.trials
        .map(_.bytes)
        .toRight(AssetError.NoTrialInventory(dataset.id))
      _ <- firstRepeat(stored.map(_.file)).map(AssetError.DuplicateAsset(_)).toLeft(())
      byFile    = stored.map(a => a.file -> a).toMap
      placement = dataset.geometry.image
      displays <- rows.traverse { r =>
        for
          kind <- DisplayKind.parse(r.trial, r.kind)
          item <- Option.when(r.item.trim.nonEmpty)(r.item).traverse(MatchItem.of(r.trial, _))
          file <- Option.when(r.file.trim.nonEmpty)(r.file).traverse(AssetFile.of)
          link = file.map(f => byFile.get(f).fold(AssetLink.Missing(f))(AssetLink.Present(_)))
          display <- Display.of(r.trial, kind, link)
        yield TrialDisplay(r.trial, item, display, placement)
      }
      registry <- of(dataset.id, inventory, dataset.geometry.screen, displays)
    yield registry

  /** That `registry` belongs to `dataset`: the same revision and the same
    * trial inventory bytes.
    */
  def check(registry: AssetRegistry, dataset: DatasetRevisionSpec): Either[AssetError, Unit] =
    for
      _ <- Either.cond(
        registry.dataset == dataset.id,
        (),
        AssetError.WrongDataset(registry.dataset, dataset.id)
      )
      source <- dataset.sources.trials
        .map(_.bytes)
        .toRight(AssetError.NoTrialInventory(dataset.id))
      _ <- Either.cond(
        registry.inventory == source,
        (),
        AssetError.WrongInventory(dataset.id, registry.inventory, source)
      )
    yield ()

  given Encoder.AsObject[AssetRegistry] =
    Encoder.forProduct4("dataset", "inventory", "screen", "trials")(r =>
      (r.dataset, r.inventory.hex, r.screen, r.trials)
    )
  given Decoder[AssetRegistry] = Decoder.instance { (c: HCursor) =>
    for
      dataset   <- c.get[DatasetRevision]("dataset")
      hex       <- c.get[String]("inventory")
      inventory <- ByteDigest.parse(hex).left.map(e => DecodingFailure(e.message, c.history))
      screen    <- c.get[ScreenSize]("screen")
      trials    <- c.get[Vector[TrialDisplay]]("trials")
      registry  <- of(dataset, inventory, screen, trials).left.map(e =>
        DecodingFailure(e.message, c.history)
      )
    yield registry
  }

  /** The versioned canonical codec, schema `studio.asset-registry@1`. Reading
    * validates exactly as [[of]] does.
    */
  val codec: Either[CodecError, VersionedCodec[AssetRegistry]] =
    StudioSchemaIds.forCodec.map { ids =>
      VersionedCodec.checked[AssetRegistry](ids.assets)(r => Right(CanonicalJson(r.asJson)))(
        json =>
          json.as[AssetRegistry].left.map(f => CodecError.Field("assets", json, f.getMessage))
      )
    }

  def encode(registry: AssetRegistry): Either[CodecError, Json] =
    codec.flatMap(_.encode(registry))

  def decode(json: Json): Either[CodecError, AssetRegistry] = codec.flatMap(_.decode(json))

  def digest(registry: AssetRegistry): Either[CodecError, CanonicalDigest[AssetRegistry]] =
    codec.flatMap(_.digest(registry))

private object AssetCodecs:
  def validated[A](of: String => Either[AssetError, A], unwrap: A => String): Codec[A] =
    Codec.from(
      Decoder[String].emap(s => of(s).left.map(_.message)),
      Encoder[String].contramap(unwrap)
    )
