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

package eyes4s.studio.desktop.data

import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.{Command, History, JournalEntry}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.preset.InitialRecipe
import eyes4s.studio.core.real.RealStudyBackend
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.platform.FileProjectStore
import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, SessionPort}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path

/** Small authored files drive real storage, native admission and host repair. */
object SourceRepairFileFixture:
  def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new AssertionError(e.toString), identity)
  def bytes(text: String): IArray[Byte] = IArray.from(text.getBytes(UTF_8))
  val fixations                         = bytes(
    "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n" +
      "P17,Encoding,enc_03,1,1,100,300,600,300,1\n" +
      "P17,Encoding,enc_04,1,1,200,100,620,320,1\n" +
      "P17,Retrieval,ret_07,1,1,50,100,604,305,1\n"
  )
  val replacement = bytes(String(Array.from(fixations), UTF_8).replace("600,300", "610,310"))
  val inventory   = bytes(
    "participant,phase,trial,occurrence,item,response,display_kind,image_file\n" +
      "P17,Encoding,enc_03,1,item-a,,image,missing.png\n" +
      "P17,Encoding,enc_04,1,item-b,,blank,\n" +
      "P17,Retrieval,ret_07,1,item-a,Remembered,blank+fixation-cross,\n"
  )
  val dataset          = DatasetRevision(1)
  val trial            = TrialKey("P17", Phase.Encoding, "enc_03", 1)
  val blank            = TrialKey("P17", Phase.Encoding, "enc_04", 1)
  private val original = get(StoryMoments.t2).datasets.last
  val sources          = get(
    Sources.of(
      Vector(
        Source(
          SourceRole.Fixations,
          get(SourcePath.of("inputs/fixations.csv")),
          ByteDigest.sha256(fixations),
          None
        ),
        Source(
          SourceRole.Trials,
          get(SourcePath.of("inputs/trials.csv")),
          ByteDigest.sha256(inventory),
          None
        )
      )
    )
  )
  val mapping = get(
    InventoryMapping.withDisplays(
      original.inventory.get,
      Some(
        DisplayColumns(
          get(ColumnName.of("display_kind")),
          Some(get(ColumnName.of("image_file")))
        )
      )
    )
  )
  val imported = get(
    History
      .start(get(AppModel.newProject).document)
      .apply(
        Command.ImportSources(
          None,
          sources,
          original.mapping,
          original.units,
          original.geometry,
          DeclaredAttributes.empty,
          None,
          Some(mapping)
        )
      )
  ).history.document
  val owner = get(LockOwner.of("Source repair filesystem acceptance"))

  def open(path: Path): Resource[IO, SessionPort] =
    for
      store   <- Resource.eval(FileProjectStore.at[IO](path))
      entries <- Resource.eval(
        Resource.make(store.acquire(owner).map(get))(lock => store.release(lock).map(get)).use {
          lock =>
            Vector(SourceRole.Fixations -> fixations, SourceRole.Trials -> inventory).traverse {
              (role, raw) =>
                ProjectBundle
                  .importInput(
                    store,
                    lock,
                    InputKind.Source(role),
                    sources.entries.find(_.role == role).get.path.value.split('/').last,
                    raw
                  )
                  .map(get)
            }
        }
      )
      session <- Resource.make(
        ProjectSession.create(store, owner, imported, SharingOptions.complete, entries).map(get)
      )(_.close.map(get))
      port <- Resource.make(IO.blocking(SessionPort.start(session)))(p =>
        IO.blocking(p.close())
      )
    yield port

  def reopen(path: Path): Resource[IO, SessionPort] =
    for
      store   <- Resource.eval(FileProjectStore.at[IO](path))
      session <- Resource
        .make(ProjectSession.open(store, owner).map(get).map(_.session))(_.close.map(get))
      port <- Resource.make(IO.blocking(SessionPort.start(session)))(p =>
        IO.blocking(p.close())
      )
    yield port

  def admit(port: SessionPort, id: DatasetRevision): IO[StudioDocument] =
    for
      _        <- port.session.perform(JournalEntry.Apply(Command.VerifyDataset(id))).map(get)
      checking <- port.session.document
      spec    = checking.dataset(id).get
      content = spec.decision match
        case AdmissionDecision.Verifying(value) => value
        case other => throw new AssertionError(s"not verifying: $other")
      summary <- RealStudyBackend
        .resource[IO](checking, DatasetSourceHosts.stored(port))
        .use(_.verify(id, content).map(get))
      _ = assert(summary.dataset == id)
      _ <- port.session
        .perform(
          JournalEntry.Apply(
            Command.Admit(
              id,
              content,
              Some(eyes4s.plan.AdmissionDecision.ReviewExclusions),
              CoreBinding.unbound,
              CoreBinding.unbound
            )
          )
        )
        .map(get)
      document <- port.session.document
    yield document

  def working(port: SessionPort, id: DatasetRevision): IO[StudioDocument] =
    for
      admitted <- admit(port, id)
      seed = get(InitialRecipe.of(admitted.dataset(id).get, Preset.EncodingRetrieval))
      _ <- port.session
        .perform(JournalEntry.Apply(Command.StartAnalysis(id, seed._1, seed._2)))
        .map(get)
      document <- port.session.document
    yield document

  def checked(port: SessionPort, document: StudioDocument): IO[AppModel] =
    for statuses <- port.session.checkInputs
    yield
      val base  = AppModel.open(document, None)
      val asked = AppModel.update(base, Intent.CheckInputs)._1
      AppModel.update(asked, Intent.InputsChecked(asked.checks.asked, statuses))._1

  def source(document: StudioDocument, id: DatasetRevision = dataset): Source =
    document.dataset(id).get.sources.fixations.get
  def address(root: Path, source: Source): Path =
    root.resolve(get(ProjectBundle.inputPath(source)).value)
