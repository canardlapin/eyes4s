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

package eyes4s.studio.desktop.shell

import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.{InputEntry, InputKind, InputStatus}
import eyes4s.studio.core.document.{Source, SourceRole}
import eyes4s.studio.desktop.compare.PanelSources
import eyes4s.studio.desktop.runtime.ProjectPort
import eyes4s.studio.desktop.trial.{GoldenTrials, StimulusSource}
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** Actual public fixture bytes and stimuli for native story captures. */
trait StoryCaptureFixture extends ShellFxSuite:
  protected val golden = PanelSources(
    GoldenTrials.contentSource,
    StimulusSource.directory(GoldenTrials.stimuli)
  )

  /** Real source bytes behind the same host port as an open project. A
    * story model alone has no filesystem project and cannot populate Data.
    */
  protected def fixtureProject: ProjectPort = new ProjectPort:
    private val paths = Vector(
      InputKind.Source(SourceRole.Fixations) -> GoldenTrials.golden.resolve("fixations.csv"),
      InputKind.Source(SourceRole.Trials)    -> GoldenTrials.golden.resolve("trials.csv")
    ) ++ {
      val listed = Files.list(GoldenTrials.stimuli)
      try
        listed.iterator.asScala.toVector
          .filter(Files.isRegularFile(_))
          .map(InputKind.StimulusImage -> _)
      finally listed.close()
    }
    private val entries = paths.map { (kind, path) =>
      val bytes  = Files.readAllBytes(path)
      val digest = ByteDigest.sha256(IArray.unsafeFromArray(bytes))
      val entry  = InputEntry
        .of(kind, path.getFileName.toString, digest, bytes.length.toLong)
        .fold(e => fail(e.message), identity)
      entry -> path
    }
    override def storedInputs(done: Either[String, Vector[InputEntry]] => Unit): Unit =
      done(Right(entries.map(_._1)))
    override def checkInputs(done: Either[String, Vector[InputStatus]] => Unit): Unit =
      val checked = entries.foldLeft[Either[String, Vector[InputStatus]]](Right(Vector.empty)) {
        case (acc, (entry, path)) =>
          for
            got   <- acc
            bytes <- scala.util.Try(Files.readAllBytes(path)).toEither.left.map(_.toString)
            digest = ByteDigest.sha256(IArray.unsafeFromArray(bytes))
          yield got :+ (if digest == entry.sha256 then InputStatus.Present(entry)
                        else InputStatus.Changed(entry, digest))
      }
      done(checked)
    def journal(entry: eyes4s.studio.core.command.JournalEntry): Unit                    = ()
    def save(done: Either[String, eyes4s.studio.core.session.SaveReceipt] => Unit): Unit =
      done(Left("Capture fixture is read-only"))
    def close(): Unit = ()
    override def readInput(source: Source, done: Either[String, IArray[Byte]] => Unit): Unit =
      val file   = source.path.value.split('/').last
      val answer = scala.util
        .Try(Files.readAllBytes(GoldenTrials.golden.resolve(file)))
        .toEither
        .left
        .map(_.toString)
        .map(IArray.unsafeFromArray(_))
      done(answer)
