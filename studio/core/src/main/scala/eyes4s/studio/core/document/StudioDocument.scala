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

package eyes4s.studio.core.document

import cats.syntax.all.*
import eyes4s.codec.{CanonicalDigest, CodecError, SchemaLadder, VersionedCodec}
import eyes4s.plan.DefinitionId
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, JobId, RunId}
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonObject}

/** Studio's schema identities, in its own `studio.` namespace, built with
  * the public `DefinitionId.of` (they are not eyes4s built-ins and not in the
  * eyes4s-laws registry). Pinned by `StudioSchemaIdsSuite`; add future studio
  * schemas here.
  */
object StudioSchemaIds:
  val DocumentName: String   = "studio.document"
  val ScienceName: String    = "studio.science"
  val JournalName: String    = "studio.journal"
  val DatasetName: String    = "studio.dataset-content"
  val ProjectName: String    = "studio.project"
  val AssetsName: String     = "studio.asset-registry"
  val RunArchiveName: String = "studio.run-archive"
  val ReportingName: String  = "studio.reporting-spec"

  final case class Ids(
      document: DefinitionId,
      science: DefinitionId,
      journal: DefinitionId,
      datasetContent: DefinitionId,
      project: DefinitionId,
      assets: DefinitionId,
      runArchive: DefinitionId,
      reporting: DefinitionId
  ) derives CanEqual:
    def all: Vector[DefinitionId] =
      Vector(document, science, journal, datasetContent, project, assets, runArchive, reporting)

  private def id(name: String, version: Int): Either[DocumentError, DefinitionId] =
    DefinitionId.of(name, version).left.map(_ => DocumentError.BadSchemaId(name, version))

  /** The whole document, presentation included; its science, whose digest is
    * the document's scientific identity; a line of the command journal
    * (S2.2); and the first version of the `.eyes` bundle manifest
    * `project.json` (S2.3), whose later versions its `SchemaLadder` adds;
    * a dataset revision's asset registry (S2.10); and a run archive's
    * index in the run store (S2.6); and a reporting spec, whose digest an
    * export cites (S8.7).
    */
  val ids: Either[DocumentError, Ids] =
    for
      document <- id(DocumentName, 1)
      science  <- id(ScienceName, 1)
      journal  <- id(JournalName, 1)
      dataset  <- id(DatasetName, 1)
      project  <- id(ProjectName, 1)
      assets   <- id(AssetsName, 1)
      archive  <- id(RunArchiveName, 1)
      report   <- id(ReportingName, 1)
    yield Ids(document, science, journal, dataset, project, assets, archive, report)

  /** The ids as a codec failure, for building codecs. */
  private[studio] def forCodec: Either[CodecError, Ids] =
    ids.left.map(e => CodecError.Unsupported("schema", e.message))

/** The canonical JSON the document writer emits: every object's members in
  * ascending key order (UTF-16 code-unit order, the same on every platform),
  * so equal values are equal documents and digest alike.
  */
object CanonicalJson:
  def apply(json: Json): Json =
    json.arrayOrObject(
      json,
      items => Json.fromValues(items.map(apply)),
      members => Json.fromFields(members.toVector.sortBy(_._1).map((k, v) => k -> apply(v)))
    )

/** Everything in a document that bears on science: dataset and analysis
  * revisions, the draft, runs, reporting specs and figures. Only a
  * [[StudioDocument]] builds one, so it is always cross-checked.
  */
final case class ScienceContent private[document] (
    datasets: Vector[DatasetRevisionSpec],
    analyses: Vector[AnalysisRevisionSpec],
    draft: Option[Draft],
    runs: Vector[RunRef],
    reporting: Vector[ReportingSpec],
    figures: Vector[FigureSpec]
) derives CanEqual

object ScienceContent:
  given Encoder.AsObject[ScienceContent] =
    Encoder.forProduct6("datasets", "analyses", "draft", "runs", "reporting", "figures")(s =>
      (s.datasets, s.analyses, s.draft, s.runs, s.reporting, s.figures)
    )

  /** The science's versioned codec; its digest is the scientific identity.
    * Reading validates exactly as a document does.
    */
  val ladder: Either[CodecError, SchemaLadder[ScienceContent]] =
    StudioSchemaIds.forCodec.map { ids =>
      def write(s: ScienceContent): Either[CodecError, Json]   = Right(CanonicalJson(s.asJson))
      def read(json: Json): Either[CodecError, ScienceContent] =
        json
          .deepMerge(
            Json.obj(
              "presentation" -> PresentationState.default.asJson,
              "jobs"         -> Json.arr()
            )
          )
          .as[StudioDocument]
          .bimap(f => CodecError.Field("science", json, f.getMessage), _.science)
      def beforeMethods(s: ScienceContent): Either[CodecError, ScienceContent] =
        Either.cond(
          s.figures.forall(_.methods.isEmpty),
          s,
          CodecError.Unsupported("studio science", "stored figure methods need version 4")
        )
      def beforeInitial(s: ScienceContent): Either[CodecError, ScienceContent] =
        beforeMethods(s).flatMap(v =>
          Either.cond(
            !s.draft.exists(_.isInitial),
            v,
            CodecError.Unsupported("studio science", "an initial draft needs version 3")
          )
        )
      def before(s: ScienceContent): Either[CodecError, ScienceContent] =
        beforeInitial(s).flatMap(_ =>
          Either.cond(
            s.reporting.forall(_.contrast.isEmpty),
            s,
            CodecError.Unsupported(
              "studio science",
              "explicit contrast operands need version 2"
            )
          )
        )
      SchemaLadder
        .of[ScienceContent]("studio science", ids.science)(s => before(s).flatMap(write))(
          json => read(json).flatMap(before)
        )
        .next(
          s =>
            s.figures.forall(_.methods.isEmpty) && !s.draft.exists(_.isInitial) && s.reporting
              .forall(_.contrast.isEmpty),
          identity
        )(s => beforeInitial(s).flatMap(write))(json => read(json).flatMap(beforeInitial))
        .next(
          s => s.figures.forall(_.methods.isEmpty) && !s.draft.exists(_.isInitial),
          identity
        )(s => beforeMethods(s).flatMap(write))(json => read(json).flatMap(beforeMethods))
        .next(_.figures.forall(_.methods.isEmpty), identity)(write)(read)
    }

  val codec: Either[CodecError, VersionedCodec[ScienceContent]] = ladder.map(_.codec)

/** An Eyes Studio project document (ticket S2.1): its science
  * ([[ScienceContent]]) and, strictly apart, its [[PresentationState]], the
  * ephemeral [[JobHandle]]s of its running runs, and its repaired display
  * assets ([[AssetRelinks]], S5.7), which no science reads.
  *
  * Built only through [[StudioDocument.of]], which checks every
  * cross-reference: ids ascend, every revision, run and reporting spec a value
  * names is present, a draft starts from its base's recipe, follows the latest
  * revision and rebases only onto an admitted dataset, a figure's panel scales
  * belong to its run's revision, and each job handle names one running run.
  */
final case class StudioDocument private (
    science: ScienceContent,
    presentation: PresentationState,
    jobs: Vector[JobHandle],
    relinks: AssetRelinks
) derives CanEqual:
  def datasets: Vector[DatasetRevisionSpec]  = science.datasets
  def analyses: Vector[AnalysisRevisionSpec] = science.analyses
  def draft: Option[Draft]                   = science.draft
  def runs: Vector[RunRef]                   = science.runs
  def reporting: Vector[ReportingSpec]       = science.reporting
  def figures: Vector[FigureSpec]            = science.figures

  def dataset(id: DatasetRevision): Option[DatasetRevisionSpec]    = datasets.find(_.id == id)
  def analysis(id: AnalysisRevision): Option[AnalysisRevisionSpec] = analyses.find(_.id == id)
  def run(id: RunId): Option[RunRef]                               = runs.find(_.id == id)

  /** The number the next created figure gets: one after the last. */
  def nextFigureId: Either[DocumentError, FigureId] =
    FigureId.of(figures.lastOption.fold(1)(_.id.number + 1))

  /** The backend job of a running run, while this session knows it. */
  def job(run: RunId): Option[JobId] = jobs.find(_.run == run).map(_.job)

  /** The latest dataset revision that has been admitted. */
  def latestAdmitted: Option[DatasetRevisionSpec] = datasets.findLast(_.decision.isAdmitted)

  def latestAnalysis: Option[AnalysisRevisionSpec] = analyses.lastOption

  def runsOn(dataset: DatasetRevision): Vector[RunRef] = runs.filter(_.dataset == dataset)

  def running: Vector[RunRef] = runs.filter(_.state == RunLifecycle.Running)

  /** The recipe the draft describes, if there is a draft. */
  def draftContext: Option[DraftContext] = draft.flatMap(_.context(analyses))
  def draftRecipe: Option[Recipe]        = draftContext.map(_.recipe)

  /** The same science with another presentation. */
  def withPresentation(next: PresentationState): Either[DocumentError, StudioDocument] =
    StudioDocument.checkPresentation(science.runs, next).as(copy(presentation = next))

  /** The same science with other job handles. */
  def withJobs(next: Vector[JobHandle]): Either[DocumentError, StudioDocument] =
    StudioDocument.checkJobs(science.runs, next).as(copy(jobs = next))

  /** The same science with other repaired assets: each names a dataset
    * revision of the document.
    */
  def withRelinks(next: AssetRelinks): Either[DocumentError, StudioDocument] =
    next.entries
      .find(r => dataset(r.dataset).isEmpty)
      .map(r => DocumentError.RelinkUnknownDataset(r.dataset, r.file.value))
      .toLeft(copy(relinks = next))

object StudioDocument:
  def of(
      datasets: Vector[DatasetRevisionSpec],
      analyses: Vector[AnalysisRevisionSpec],
      draft: Option[Draft],
      runs: Vector[RunRef],
      reporting: Vector[ReportingSpec],
      figures: Vector[FigureSpec],
      presentation: PresentationState,
      jobs: Vector[JobHandle]
  ): Either[DocumentError, StudioDocument] =
    val datasetIds  = datasets.map(_.id)
    val analysisIds = analyses.map(_.id)
    val reportingId = reporting.map(_.id)
    def known[A](ids: Vector[A], id: A)(error: => DocumentError)(using CanEqual[A, A]) =
      Either.cond(ids.contains(id), (), error)
    for
      _ <- DocumentCodecs.ascending("dataset revision", datasets)(_.id.number)(_.id.label)
      _ <- DocumentCodecs.ascending("analysis revision", analyses)(_.id.number)(_.id.label)
      _ <- DocumentCodecs.ascending("run", runs)(_.id.number)(_.id.label)
      _ <- DocumentCodecs.ascending("figure", figures)(_.id.number)(_.id.label)
      _ <- Either.cond(
        reportingId.zip(reportingId.drop(1)).forall((a, b) => a.value < b.value),
        (),
        DocumentError.UnorderedIds("reporting spec", reportingId.map(_.value))
      )
      _ <- datasets.traverse_(DatasetRevisionSpec.checkAttributes)
      _ <- datasets.traverse_(DatasetRevisionSpec.checkInventory)
      _ <- datasets.traverse_ { d =>
        d.parent.traverse_ { p =>
          if p.number >= d.id.number then Left(DocumentError.ParentNotEarlier(d.id, p))
          else known(datasetIds, p)(DocumentError.UnknownDataset(s"dataset ${d.id.label}", p))
        }
      }
      _ <- analyses.traverse_ { a =>
        known(datasetIds, a.dataset)(DocumentError.UnknownDataset(a.id.label, a.dataset))
      }
      _ <- draft.traverse_ { d =>
        val referrer = s"draft ${d.id.label}"
        val seed     = d.origin match
          case DraftOrigin.Existing(baseId) =>
            analyses
              .find(_.id == baseId)
              .toRight(DocumentError.UnknownAnalysis(referrer, baseId))
              .flatMap { base =>
                val latest = analyses.lastOption.getOrElse(base)
                Either.cond(
                  d.id.number > latest.id.number,
                  (),
                  DocumentError.DraftNotLatest(d.id, latest.id)
                ) *>
                  d.check(base)
              }
          case DraftOrigin.Initial(seedDataset, _, _) =>
            for
              _ <- Either.cond(
                analyses.isEmpty,
                (),
                DocumentError.InitialDraftWithAnalyses(d.id, analysisIds)
              )
              _ <- Either.cond(
                d.id == AnalysisRevision(1),
                (),
                DocumentError.InitialDraftId(d.id, AnalysisRevision(1))
              )
              _ <- datasets
                .find(_.id == seedDataset)
                .toRight(DocumentError.UnknownDataset(referrer, seedDataset))
                .flatMap(data =>
                  Either.cond(
                    data.decision.isAdmitted,
                    (),
                    DocumentError.RebaseNotAdmitted(d.id, seedDataset)
                  )
                )
            yield ()
        seed *> d.dataset.traverse_ { target =>
          datasets.find(_.id == target) match
            case None => Left(DocumentError.UnknownDataset(referrer, target))
            case Some(data) if !data.decision.isAdmitted =>
              Left(DocumentError.RebaseNotAdmitted(d.id, target))
            case Some(_) => Right(())
        }
      }
      _ <- runs.traverse_ { r =>
        known(analysisIds, r.analysis)(DocumentError.UnknownAnalysis(r.id.label, r.analysis)) *>
          known(datasetIds, r.dataset)(DocumentError.UnknownDataset(r.id.label, r.dataset))
      }
      _ <- figures.traverse_ { f =>
        for
          run <- runs.find(_.id == f.run).toRight(DocumentError.UnknownRun(f.id.label, f.run))
          _   <- known(reportingId, f.reporting)(
            DocumentError.UnknownReporting(f.id.label, f.reporting)
          )
          scales = analyses.find(_.id == run.analysis).map(_.recipe.scales.values)
          _ <- f.namedScales.traverse_ { (letter, sigma) =>
            Either.cond(
              scales.exists(_.contains(sigma)),
              (),
              DocumentError.PanelScaleNotInRun(
                f.id,
                letter,
                sigma,
                run.id,
                scales.getOrElse(Vector.empty)
              )
            )
          }
        yield ()
      }
      _ <- checkPresentation(runs, presentation)
      _ <- checkJobs(runs, jobs)
    yield new StudioDocument(
      ScienceContent(datasets, analyses, draft, runs, reporting, figures),
      presentation,
      jobs.sortBy(_.run.number),
      AssetRelinks.empty
    )

  private[document] def checkPresentation(
      runs: Vector[RunRef],
      presentation: PresentationState
  ): Either[DocumentError, Unit] =
    presentation.shownRun.traverse_ { r =>
      Either.cond(runs.exists(_.id == r), (), DocumentError.UnknownRun("the presentation", r))
    }

  /** Each handle names a running run; a run has at most one. A running run
    * may have none (its handle was lost with the session).
    */
  private[document] def checkJobs(
      runs: Vector[RunRef],
      jobs: Vector[JobHandle]
  ): Either[DocumentError, Unit] =
    for
      _ <- jobs.traverse_ { h =>
        runs.find(_.id == h.run) match
          case None => Left(DocumentError.UnknownRun("a job handle", h.run))
          case Some(r) if r.state != RunLifecycle.Running =>
            Left(DocumentError.JobNotRunning(h.run, h.job))
          case Some(_) => Right(())
      }
      _ <- jobs.groupBy(_.run).toVector.sortBy(_._1.number).traverse_ { (run, hs) =>
        Either.cond(hs.size == 1, (), DocumentError.DuplicateJobs(run, hs.map(_.job)))
      }
    yield ()

  /** `relinks` is written only when the document has repaired an asset, so
    * a document without one is written as before S5.7, byte for byte.
    */
  given Encoder.AsObject[StudioDocument] = Encoder.AsObject.instance { d =>
    val o = d.science.asJsonObject
      .add("presentation", d.presentation.asJson)
      .add("jobs", d.jobs.asJson)
    if d.relinks.isEmpty then o else o.add("relinks", d.relinks.asJson)
  }

  given Decoder[StudioDocument] = Decoder.instance { c =>
    for
      // Family persistence and safe interpretation land together; bead q-analysis-family-identity.
      _ <- Either.cond(
        !c.downField("analysisFamilies").succeeded,
        (),
        DecodingFailure(
          "Document field analysisFamilies requires supported family ownership and interpretation.",
          c.history
        )
      )
      document <- Decoder
        .forProduct8(
          "datasets",
          "analyses",
          "draft",
          "runs",
          "reporting",
          "figures",
          "presentation",
          "jobs"
        )(of)
        .emap(_.left.map(_.message))
        .apply(c)
      relinks <- c.getOrElse[AssetRelinks]("relinks")(AssetRelinks.empty)
      full <- document.withRelinks(relinks).left.map(e => DecodingFailure(e.message, c.history))
    yield full
  }

  /** The presets a version-4 document knows: every one before
    * `PerceptionImagery` (S7.1), which only version 5 can name.
    */
  private def presetBeforeV5(preset: Preset): Boolean = preset != Preset.PerceptionImagery

  /** Whether a version-4 document can hold `document`: no analysis names a
    * preset added in version 5.
    */
  private def expressedByV4(document: StudioDocument): Boolean =
    expressedByV5(document) && document.analyses.forall(a => presetBeforeV5(a.studio.preset))

  /** Version 6 first records explicit ordered reporting contrast operands. */
  private def expressedByV7(document: StudioDocument): Boolean =
    document.figures.forall(_.methods.isEmpty)
  private def beforeV8(document: StudioDocument): Either[CodecError, StudioDocument] =
    Either.cond(
      expressedByV7(document),
      document,
      CodecError.Unsupported("studio document", "stored figure methods need version 8")
    )
  private def expressedByV6(document: StudioDocument): Boolean =
    expressedByV7(document) && !document.draft.exists(_.isInitial)
  private def beforeV7(document: StudioDocument): Either[CodecError, StudioDocument] =
    beforeV8(document).flatMap(v =>
      Either.cond(
        expressedByV6(document),
        v,
        CodecError.Unsupported("studio document", "an initial draft needs version 7")
      )
    )
  private def expressedByV5(document: StudioDocument): Boolean =
    expressedByV6(document) && document.reporting.forall(_.contrast.isEmpty)

  private val contrastVersionError: CodecError =
    CodecError.Unsupported("studio document", "explicit contrast operands need version 6")

  private def beforeV6(document: StudioDocument): Either[CodecError, StudioDocument] =
    beforeV7(document).flatMap(value =>
      Either.cond(expressedByV5(value), value, contrastVersionError)
    )

  /** A version-1 to -4 writer and reader: an enum value cannot be dropped as
    * a member can, so a document naming a version-5 preset is refused both
    * ways rather than written or read under a version that cannot name it.
    */
  private def beforeV5(
      write: StudioDocument => Json
  ): StudioDocument => Either[CodecError, Json] =
    d =>
      beforeV6(d).flatMap { value =>
        if expressedByV4(value) then Right(write(value))
        else Left(CodecError.Unsupported("studio document", "a preset needs version 5"))
      }

  private def readBeforeV5(
      read: Json => Either[CodecError, StudioDocument]
  ): Json => Either[CodecError, StudioDocument] =
    json =>
      read(json)
        .flatMap(beforeV6)
        .filterOrElse(
          expressedByV4,
          CodecError.Unsupported("studio document", "a preset needs version 5")
        )

  /** Whether a version-3 document can hold `document`: version 3 records
    * neither display columns nor repaired assets (S5.7).
    */
  private def expressedByV3(document: StudioDocument): Boolean =
    expressedByV4(document) && document.relinks.isEmpty &&
      document.datasets.forall(_.inventory.forall(_.displays.isEmpty))

  /** Whether a version-2 document can hold `document`: version 2 records no
    * admission policy (S5.6).
    */
  private def expressedByV2(document: StudioDocument): Boolean =
    expressedByV3(document) && document.datasets.forall(_.decision.admittedUnder.isEmpty)

  /** Whether a version-1 document can hold `document`: version 1 records
    * neither a trial inventory mapping (S5.4) nor an admission policy.
    */
  private def expressedByV1(document: StudioDocument): Boolean =
    expressedByV2(document) && document.datasets.forall(_.inventory.isEmpty)

  private def mapDatasets(payload: Json)(f: JsonObject => JsonObject): Json =
    payload.hcursor
      .downField("datasets")
      .withFocus(_.mapArray(_.map(_.mapObject(f))))
      .top
      .getOrElse(payload)

  /** `payload` without any admitted revision's policy: what a version-2
    * reader saw, since it did not know the member.
    */
  private def withoutPolicy(payload: Json): Json =
    mapDatasets(payload)(d =>
      d("decision").fold(d)(decision =>
        d.add("decision", decision.mapObject(_.mapValues(_.mapObject(_.remove("policy")))))
      )
    )

  /** `payload` without repaired assets or display columns: what a version-3
    * reader saw, since it did not know the members.
    */
  private def withoutAssets(payload: Json): Json =
    mapDatasets(payload.mapObject(_.remove("relinks")))(d =>
      d("inventory").fold(d)(i => d.add("inventory", i.mapObject(_.remove("displays"))))
    )

  /** `payload` without any dataset revision's inventory mapping or policy:
    * what a version-1 reader saw, since it did not know the members.
    */
  private def withoutInventory(payload: Json): Json =
    mapDatasets(withoutPolicy(payload))(_.remove("inventory"))

  private def read(json: Json): Either[CodecError, StudioDocument] =
    json.as[StudioDocument].left.map(f => CodecError.Field("document", json, f.getMessage))

  /** Every version of the document schema; a later version is added with
    * `SchemaLadder.next` and its upcast, and `ladder.lift` rewrites a stored
    * document as the latest version (CR3).
    *
    * Version 2 (S5.4) adds a dataset revision's trial inventory mapping
    * (`inventory`, written only when a revision has one). A document with no
    * mapping is still written as version 1, byte for byte; one with a
    * mapping is version 2, which a version-1 reader refuses
    * (`CodecError.UnsupportedSchema`) instead of dropping the mapping. The
    * upcast is the identity: a version-1 document has no mapping.
    *
    * Version 3 (S5.6) adds the eyes4s `AdmissionDecision` an admitted
    * revision was admitted under (`decision.Admitted.policy`, written only
    * when recorded), in the same way: a document that records no policy is
    * still written as version 1 or 2, and the upcast is the identity, since
    * an earlier document records none.
    *
    * Version 4 (S5.7) adds a trial inventory's display columns
    * (`inventory.displays`) and the document's repaired display assets
    * (`relinks`), each written only when present, in the same way.
    *
    * Version 5 (S7.1) adds the `PerceptionImagery` preset. A document whose
    * analyses name none is still written as version 4 or earlier, byte for
    * byte, and the upcast is the identity. One that names it is version 5,
    * which an earlier reader refuses; earlier writers and readers refuse it
    * too, since a preset cannot be dropped.
    *
    * Version 6 records explicitly ordered reporting contrast operands. Earlier
    * specs lift unchanged with no contrast; grouping still serves group means.
    * Earlier readers and writers refuse explicit operands, so relabelling an
    * envelope cannot change scientific direction.
    */
  // Version 7 alone expresses a working recipe without a saved base.
  val ladder: Either[CodecError, SchemaLadder[StudioDocument]] =
    StudioSchemaIds.forCodec.map { ids =>
      SchemaLadder
        .of[StudioDocument]("studio document", ids.document)(
          beforeV5(d => CanonicalJson(withoutInventory(d.asJson)))
        )(readBeforeV5(json => read(withoutInventory(json))))
        .next(expressedByV1, identity)(beforeV5(d => CanonicalJson(withoutPolicy(d.asJson))))(
          readBeforeV5(json => read(withoutPolicy(json)))
        )
        .next(expressedByV2, identity)(beforeV5(d => CanonicalJson(withoutAssets(d.asJson))))(
          readBeforeV5(json => read(withoutAssets(json)))
        )
        .next(expressedByV3, identity)(beforeV5(d => CanonicalJson(d.asJson)))(
          readBeforeV5(read)
        )
        .next(expressedByV4, identity)(d => beforeV6(d).map(v => CanonicalJson(v.asJson)))(
          json => read(json).flatMap(beforeV6)
        )
        .next(expressedByV5, identity)(d => beforeV7(d).map(v => CanonicalJson(v.asJson)))(
          json => read(json).flatMap(beforeV7)
        )
        .next(expressedByV6, identity)(d => beforeV8(d).map(v => CanonicalJson(v.asJson)))(
          json => read(json).flatMap(beforeV8)
        )
        .next(expressedByV7, identity)(d => Right(CanonicalJson(d.asJson)))(read)
    }

  /** The versioned, canonical document codec. */
  val codec: Either[CodecError, VersionedCodec[StudioDocument]] = ladder.map(_.codec)

  /** The stored document: the schema envelope and canonical JSON. */
  def encode(document: StudioDocument): Either[CodecError, Json] =
    codec.flatMap(_.encode(document))

  def decode(json: Json): Either[CodecError, StudioDocument] = codec.flatMap(_.decode(json))

  def parse(text: String): Either[CodecError, StudioDocument] = codec.flatMap(_.parse(text))

  /** The document's scientific identity: the CR3 digest of its science,
    * which no presentation change or job handle can alter.
    */
  def scienceDigest(
      document: StudioDocument
  ): Either[CodecError, CanonicalDigest[ScienceContent]] =
    ScienceContent.codec.flatMap(_.digest(document.science))
