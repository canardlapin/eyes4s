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

package eyes4s.studio.app.about

import eyes4s.studio.app.text.Messages
import eyes4s.studio.core.backend.ProtocolVersion
import eyes4s.studio.core.engine.StudioBuild

/** One third-party component the build ships: a resolved library or a
  * bundled file, with its version and licence (an SPDX expression).
  */
final case class AboutComponent(
    kind: ComponentKind,
    group: String,
    name: String,
    version: String,
    licence: String,
    copyright: String
) derives CanEqual

enum ComponentKind derives CanEqual:
  case Library, Bundled

/** What the host knows and the build does not: the running Java and JavaFX,
  * and the components the build's generated notices list.
  */
final case class AboutFacts(
    javaVersion: String,
    javaVendor: String,
    javaFxVersion: Option[String],
    components: Either[String, Vector[AboutComponent]]
) derives CanEqual

/** One line of the About box: its label and value. */
final case class AboutLine(label: String, value: String) derives CanEqual

/** One row of the components table. */
final case class ComponentRow(name: String, version: String, licence: String) derives CanEqual

/** The About box (ticket S1.14): the build's version, commit and protocol,
  * the Java and JavaFX it runs on, and every bundled component with its
  * version and licence. `problem` says why the components are not listed.
  */
final case class AboutVM(
    title: String,
    lines: Vector[AboutLine],
    componentsTitle: String,
    components: Vector[ComponentRow],
    problem: Option[String],
    notices: String,
    close: String
) derives CanEqual

/** The About box's strings. Templates name their arguments by position, as
  * [[Messages]] does.
  */
enum AboutTextId derives CanEqual:
  case Title, Version, VersionValue, Commit, NotGit, Protocol, Java, JavaValue, JavaFx
  case JavaFxValue, JavaFxNotRunning, Components, ComponentsOf, Unlisted, Notices, Close
  case FirstParty, Dirty

object AboutText:
  import AboutTextId.*

  def english(id: AboutTextId): String = id match
    case Title            => "About Eyes Studio"
    case Version          => "Version"
    case VersionValue     => "eyes4s {0}"
    case Commit           => "Commit"
    case NotGit           => "not built from a Git checkout"
    case Protocol         => "Backend protocol"
    case Java             => "Java"
    case JavaValue        => "{0} ({1})"
    case JavaFx           => "JavaFX"
    case JavaFxValue      => "{0} (built against {1})"
    case JavaFxNotRunning => "not running (built against {0})"
    case Components       => "Bundled components"
    case ComponentsOf     => "Components ({0}, Eyes Studio included)"
    case Unlisted         => "The component list could not be read: {0}"
    case Notices          =>
      "Licences and notices: THIRD-PARTY.txt, THIRD-PARTY-COMPONENTS.txt and licences/ " +
        "in the application's notices folder."
    case Close      => "Close"
    case FirstParty => "Eyes Studio and eyes4s"
    case Dirty      => "{0} with uncommitted changes"

  def apply(id: AboutTextId, args: String*): String = Messages.fill(english(id), args.toVector)

object AboutBox:
  import AboutTextId.*

  /** `components.tsv` as the build writes it (`StudioNotices`): kind, group,
    * name, version, licence, copyright; one component per line.
    */
  def parse(tsv: String): Either[String, Vector[AboutComponent]] =
    val rows = tsv.linesIterator.zipWithIndex
      .filter(_._1.trim.nonEmpty)
      .map { (line, i) =>
        line.split("\t", -1).toVector match
          case Vector(kind, group, name, version, licence, copyright) =>
            (kind match
              case "dependency" => Right(ComponentKind.Library)
              case "bundled"    => Right(ComponentKind.Bundled)
              case other        => Left(s"line ${i + 1} has the kind '$other'")
            ).map(AboutComponent(_, group, name, version, licence, copyright))
          case cells => Left(s"line ${i + 1} has ${cells.size} cells, not 6")
      }
      .toVector
    rows.collectFirst { case Left(e) => e } match
      case Some(e) => Left(e)
      case None    =>
        val all = rows.collect { case Right(c) => c }
        if all.isEmpty then Left("it lists no component") else Right(all)

  /** The commit line: the first 12 characters of the commit, and a dirty
    * build said so, never shown as the commit it is not.
    */
  def commitLine(commit: Option[String], dirty: Boolean): String =
    commit.fold(AboutText(NotGit)) { c =>
      val short = c.take(12)
      if dirty then AboutText(Dirty, short) else short
    }

  /** The About box for `facts`, with the generated build's version, commit
    * and protocol.
    */
  def vm(facts: AboutFacts): AboutVM =
    val commit = commitLine(StudioBuild.commit, StudioBuild.dirty)
    val javaFx =
      facts.javaFxVersion.fold(AboutText(JavaFxNotRunning, StudioBuild.javaFxVersion))(v =>
        AboutText(JavaFxValue, v, StudioBuild.javaFxVersion)
      )
    val listed  = facts.components.toOption.getOrElse(Vector.empty)
    val ordered = listed.sortBy(c => (c.kind.ordinal, c.group, c.name))
    AboutVM(
      AboutText(Title),
      Vector(
        AboutLine(AboutText(Version), AboutText(VersionValue, StudioBuild.eyes4sBaseVersion)),
        AboutLine(AboutText(Commit), commit),
        AboutLine(AboutText(Protocol), ProtocolVersion.Current.render),
        AboutLine(AboutText(Java), AboutText(JavaValue, facts.javaVersion, facts.javaVendor)),
        AboutLine(AboutText(JavaFx), javaFx)
      ),
      if listed.isEmpty then AboutText(Components)
      else AboutText(ComponentsOf, (listed.size + 1).toString),
      ComponentRow(AboutText(FirstParty), StudioBuild.eyes4sBaseVersion, StudioBuild.licence) +:
        ordered.map(c =>
          ComponentRow(
            c.kind match
              case ComponentKind.Library => s"${c.group}:${c.name}"
              case ComponentKind.Bundled => s"${c.name} (${c.group})"
            ,
            c.version,
            c.licence
          )
        ),
      facts.components.left.toOption.map(AboutText(Unlisted, _)),
      AboutText(Notices),
      AboutText(Close)
    )
