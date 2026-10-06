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

package eyes4s.studio.desktop.journey

import cats.arrow.FunctionK
import cats.effect.IO
import cats.syntax.all.*
import eyes4s.studio.app.{AppModel, ProjectName}
import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.figures.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{FigureId, ScienceContent, StudioDocument}
import eyes4s.studio.core.figures.{FigureSource, MethodsReads}
import eyes4s.studio.core.headless.NativeHeadlessSession
import eyes4s.studio.core.navigation.StudyNavigator
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import eyes4s.studio.desktop.figures.BundleFiles
import eyes4s.studio.desktop.runtime.StudioSession
import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.Future

/** Exact native readback/export evidence shared by command and FX routes. */
object NativeCommandJourneyReadback:
  import NativeCommandJourneyFixture.*
  final case class Port(
      result: RunId => IO[Either[BackendError, ResultSummary]],
      queries: (RunId, PageRequest) => IO[Either[BackendError, QueryPage]],
      report: (RunId, eyes4s.studio.core.document.ReportingSpec, Int) => IO[
        Either[BackendError, ReportView]
      ],
      inspect: (RunId, ResultAddress) => IO[Either[BackendError, Inspection]],
      provenance: (RunId, ResultAddress) => IO[Either[BackendError, Provenance]],
      source: (AnalysisRevision, Int, Int) => IO[Either[BackendError, SourceRecordPage]],
      pairs: (RunId, Int, PageRequest) => IO[Either[BackendError, PairRowPage]],
      admission: DatasetRevision => IO[Either[BackendError, AdmissionSummary]],
      navigator: StudyNavigator[IO]
  )
  object Port:
    def from(session: NativeHeadlessSession): Port =
      def io[A](future: => Future[A]): IO[A] = IO.fromFuture(IO(future))
      val lift                               = new FunctionK[Future, IO]:
        def apply[A](future: Future[A]): IO[A] = io(future)
      Port(
        id => io(session.result(id)),
        (id, page) => io(session.queries(id, page)),
        (id, spec, scale) => io(session.report(id, spec, scale)),
        (id, address) => io(session.inspect(id, address)),
        (id, address) => io(session.provenance(id, address)),
        (revision, start, count) => io(session.sourceRecords(revision, start, count)),
        (id, scale, page) => io(session.pairRows(id, scale, page)),
        id => io(session.admission(id)),
        StudyNavigator.mapK(session.navigator)(lift)
      )
    def from(session: StudioSession): Port = Port(
      session.reads.result,
      session.reads.queries,
      session.reads.report,
      session.reads.inspect,
      session.reads.provenance,
      session.backend.sourceRecords,
      session.reads.pairRows,
      session.backend.admission,
      session.navigator
    )

  final case class Output(
      document: StudioDocument,
      canonicalDocument: Vector[Byte],
      canonicalScience: Vector[Byte],
      summary: ResultSummary,
      rows: Vector[QueryRow],
      report: ReportView,
      inspected: Inspection,
      source: SourceRecordPage,
      sourceRef: StudioRef,
      provenance: Provenance,
      exports: Vector[(String, Vector[Byte])]
  )
  private def ok[E, A](value: IO[Either[E, A]]): IO[A] = value.map(get)

  def capture(document: StudioDocument, port: Port): IO[Output] =
    val selected = get(ScaleIndex.of(scale))
    for
      summary    <- ok(port.result(run))
      rows       <- ok(MethodsReads.queryRows[IO](port.queries, run))
      report     <- ok(port.report(run, reporting, scale))
      inspected  <- ok(port.inspect(run, ResultAddress.ContrastRow(scale, focus)))
      provenance <- ok(port.provenance(run, ResultAddress.ContrastRow(scale, focus)))
      pairs      <- ok(
        port.navigator.pairs(
          StudioRef.QueryContrast(run, selected, focus),
          PairDesign.Matched,
          get(PageRequest.of(0, 1))
        )
      )
      pair = pairs.entries.headOption.getOrElse(
        throw new AssertionError("No native matched pair")
      )
      maps      <- ok(port.navigator.maps(pair))
      fixations <- ok(port.navigator.fixations(maps.query, get(PageRequest.of(0, 1))))
      fixation = fixations.entries.headOption.getOrElse(
        throw new AssertionError("No native fixation")
      )
      sourceRef <- ok(port.navigator.record(fixation))
      record = sourceRef match
        case StudioRef.SourceRecord(_, _, _, number) => number.value
        case other => throw new AssertionError(s"Unexpected native source reference $other")
      source   <- ok(port.source(revision, record, 1))
      pairRows <- ok(MethodsReads.pairRows[IO](port.pairs, run, summary.scales.size))
      facts    <- ok(MethodsReads.read[IO](port.admission, port.queries, run, dataset))
      model   = AppModel.open(document, Some(get(ProjectName.of("Native command journey"))))
      initial = FigureComposer.sync(FigureComposer.empty, model)
      replies <- initial._2
        .collect { case ComposerEffect.RequestReport(run, spec, selected) =>
          (run, spec, selected)
        }
        .traverse { (run, spec, selected) =>
          ok(port.report(run, spec, selected.value))
            .map(r => ComposerIntent.ReportRead(run, spec, selected, Right(r)))
        }
      composer = (Vector(
        ComposerIntent.SummaryRead(run, SummaryAnswer.Answered(summary)),
        ComposerIntent.Methods(MethodsIntent.FactsRead(run, Right(facts)))
      ) ++ replies)
        .foldLeft(initial._1)((c, reply) => FigureComposer.update(c, model, reply)._1)
      page = FigureComposer
        .view(composer, model)
        .page
        .getOrElse(throw new AssertionError("No native figure page"))
      bound   = get(FigureSource.of(document, get(FigureId.of(1))))
      methods = FigureComposer.view(composer, model).methods.flatMap(_.text.toOption)
      request = BundleRequest(
        bound,
        page,
        ExportFormat.Svg,
        methods,
        Vector(
          BundleItem.Figure,
          BundleItem.Results,
          BundleItem.Comparisons,
          BundleItem.Participants,
          BundleItem.Methods
        ),
        false,
        FigureBundle.folder(page),
        participantScale = Some(selected)
      )
      files = get(BundleFiles.assemble(request, summary, rows, pairRows, report = Some(report)))
      documentBytes = get(StudioDocument.encode(document)).noSpaces.getBytes(UTF_8).toVector
      scienceBytes  = get(get(ScienceContent.codec).encode(document.science)).noSpaces
        .getBytes(UTF_8)
        .toVector
    yield Output(
      document,
      documentBytes,
      scienceBytes,
      summary,
      rows,
      report,
      inspected,
      source,
      sourceRef,
      provenance,
      files.map((name, data) => name -> Vector.from(data))
    )
