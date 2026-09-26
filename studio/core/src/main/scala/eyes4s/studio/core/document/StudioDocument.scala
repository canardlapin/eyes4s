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
import io.circe.{Decoder, Encoder, Json}

/** Studio's schema identities, in its own `studio.` namespace, built with
  * the public `DefinitionId.of` (they are not eyes4s built-ins and not in the
  * eyes4s-laws registry). Pinned by `StudioSchemaIdsSuite`; add future studio
  * schemas here.
  */
object StudioSchemaIds:
  val DocumentName: String = "studio.document"
  val ScienceName: String  = "studio.science"

  final case class Ids(document: DefinitionId, science: DefinitionId) derives CanEqual:
    def all: Vector[DefinitionId] = Vector(document, science)

  private def id(name: String, version: Int): Either[DocumentError, DefinitionId] =
    DefinitionId.of(name, version).left.map(_ => DocumentError.BadSchemaId(name, version))

  /** The whole document, presentation included (`project.json` in S2.3), and
    * its science, whose digest is the document's scientific identity.
    */
  val ids: Either[DocumentError, Ids] =
    for
      document <- id(DocumentName, 1)
      science  <- id(ScienceName, 1)
    yield Ids(document, science)

  /** The ids as a codec failure, for building codecs. */
  private[document] def forCodec: Either[CodecError, Ids] =
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
  val codec: Either[CodecError, VersionedCodec[ScienceContent]] =
    StudioSchemaIds.forCodec.map { ids =>
      VersionedCodec.checked[ScienceContent](ids.science)(s => Right(CanonicalJson(s.asJson))) {
        json =>
          json
            .deepMerge(
              Json.obj(
                "presentation" -> PresentationState.default.asJson,
                "jobs"         -> Json.arr()
              )
            )
            .as[StudioDocument]
            .bimap(f => CodecError.Field("science", json, f.getMessage), _.science)
      }
    }

/** An Eyes Studio project document (ticket S2.1): its science
  * ([[ScienceContent]]) and, strictly apart, its [[PresentationState]] and
  * the ephemeral [[JobHandle]]s of its running runs.
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
    jobs: Vector[JobHandle]
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

  /** The backend job of a running run, while this session knows it. */
  def job(run: RunId): Option[JobId] = jobs.find(_.run == run).map(_.job)

  /** The latest dataset revision that has been admitted. */
  def latestAdmitted: Option[DatasetRevisionSpec] = datasets.findLast(_.decision.isAdmitted)

  def latestAnalysis: Option[AnalysisRevisionSpec] = analyses.lastOption

  def runsOn(dataset: DatasetRevision): Vector[RunRef] = runs.filter(_.dataset == dataset)

  def running: Vector[RunRef] = runs.filter(_.state == RunLifecycle.Running)

  /** The recipe the draft describes, if there is a draft. */
  def draftRecipe: Option[Recipe] =
    draft.flatMap(d => analysis(d.base).map(base => d.recipe(base.recipe)))

  /** The same science with another presentation. */
  def withPresentation(next: PresentationState): Either[DocumentError, StudioDocument] =
    StudioDocument.checkPresentation(science.runs, next).as(copy(presentation = next))

  /** The same science with other job handles. */
  def withJobs(next: Vector[JobHandle]): Either[DocumentError, StudioDocument] =
    StudioDocument.checkJobs(science.runs, next).as(copy(jobs = next))

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
        for
          base <- analyses
            .find(_.id == d.base)
            .toRight(DocumentError.UnknownAnalysis(referrer, d.base))
          latest = analyses.lastOption.getOrElse(base)
          _ <- Either.cond(
            d.id.number > latest.id.number,
            (),
            DocumentError.DraftNotLatest(d.id, latest.id)
          )
          _ <- d.dataset.traverse_ { target =>
            datasets.find(_.id == target) match
              case None => Left(DocumentError.UnknownDataset(referrer, target))
              case Some(t) if !t.decision.isAdmitted =>
                Left(DocumentError.RebaseNotAdmitted(d.id, target))
              case Some(_) => Right(())
          }
          _ <- d.check(base)
        yield ()
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
      jobs.sortBy(_.run.number)
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

  given Encoder.AsObject[StudioDocument] = Encoder.AsObject.instance { d =>
    d.science.asJsonObject
      .add("presentation", d.presentation.asJson)
      .add("jobs", d.jobs.asJson)
  }

  given Decoder[StudioDocument] =
    Decoder
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

  /** Every version of the document schema; a later version is added with
    * `SchemaLadder.next` and its upcast, and `ladder.lift` rewrites a stored
    * document as the latest version (CR3).
    */
  val ladder: Either[CodecError, SchemaLadder[StudioDocument]] =
    StudioSchemaIds.forCodec.map { ids =>
      SchemaLadder.of[StudioDocument]("studio document", ids.document)(d =>
        Right(CanonicalJson(d.asJson))
      )(json =>
        json.as[StudioDocument].left.map(f => CodecError.Field("document", json, f.getMessage))
      )
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
