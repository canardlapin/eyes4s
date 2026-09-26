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

package eyes4s.studio.core.bundle

/** The top-level directories of an `.eyes` bundle (ticket S2.3). Everything a
  * bundle stores, apart from its `project.json` manifest, lies in exactly one
  * of them.
  *
  *  - `inputs/`: copied source files, each at `inputs/<sha256>/<name>`, so a
  *    copy is addressed by the digest of its exact bytes and never rewritten;
  *  - `mappings/`, `datasets/`, `analyses/`, `runs/<id>/`, `reporting/`,
  *    `figures/`: the document's parts, each an immutable file whose name
  *    carries a prefix of its own digest;
  *  - `cache/`: disposable. The manifest never names it, so deleting it loses
  *    nothing scientific.
  */
enum BundleArea(val directory: String, val disposable: Boolean) derives CanEqual:
  case Inputs    extends BundleArea("inputs", false)
  case Mappings  extends BundleArea("mappings", false)
  case Datasets  extends BundleArea("datasets", false)
  case Analyses  extends BundleArea("analyses", false)
  case Runs      extends BundleArea("runs", false)
  case Reporting extends BundleArea("reporting", false)
  case Figures   extends BundleArea("figures", false)
  case Cache     extends BundleArea("cache", true)

object BundleArea:
  def named(directory: String): Option[BundleArea] = values.find(_.directory == directory)

/** Why a text is not a bundle path. */
enum PathProblem derives CanEqual:
  case Blank

  /** A leading `/` or `\`, or a drive letter such as `C:`. */
  case Absolute

  /** A `..` segment. */
  case Traversal
  case CurrentSegment
  case EmptySegment

  /** A segment beginning with `.`: such names are reserved for a store's own
    * files (its lock and its staged writes).
    */
  case Hidden(segment: String)

  /** A backslash, a colon or a control character, at its index. */
  case Reserved(character: Char, index: Int)
  case OutsideAreas(first: String)

  /** The path names an area directory and nothing inside it. */
  case NoName(area: BundleArea)

  def describe: String = this match
    case Blank              => "it is blank"
    case Absolute           => "it is absolute; bundle paths are relative to the bundle root"
    case Traversal          => "it has a '..' segment, which could leave the bundle"
    case CurrentSegment     => "it has a '.' segment"
    case EmptySegment       => "it has an empty segment"
    case Hidden(segment)    => s"segment '$segment' begins with '.', which the store reserves"
    case Reserved(c, index) => f"it has the reserved character U+${c.toInt}%04X at index $index"
    case OutsideAreas(first) =>
      s"'$first' is not a bundle area (${BundleArea.values.map(_.directory).mkString(", ")})"
    case NoName(area) => s"it names the ${area.directory}/ area but no entry in it"

/** A path inside an `.eyes` bundle: relative, `/`-separated, inside one
  * [[BundleArea]], with no empty, `.`, `..` or hidden segment and no
  * backslash, colon or control character. It can therefore name neither a
  * file outside the bundle, nor the manifest, nor a store's own files.
  */
final case class BundlePath private (value: String, area: BundleArea) derives CanEqual:
  def segments: Vector[String] = value.split('/').toVector
  def name: String             = segments.last

  /** Whether `this` equals `prefix` or lies under it. */
  def within(prefix: BundlePath): Boolean =
    value == prefix.value || value.startsWith(prefix.value + "/")

  override def toString: String = value

object BundlePath:
  given Ordering[BundlePath] = Ordering.by(_.value)

  def of(value: String): Either[BundleError, BundlePath] =
    def refuse(problem: PathProblem) = Left(BundleError.BadPath(value, problem))
    val reserved                     = value.zipWithIndex.collectFirst {
      case (c, i) if c == '\\' || c == ':' || c < ' ' || c == '\u007f' =>
        PathProblem.Reserved(c, i)
    }
    val segments = value.split("/", -1).toVector
    if value.trim.isEmpty then refuse(PathProblem.Blank)
    else if value.startsWith("/") || value.startsWith("\\") || isDrive(value) then
      refuse(PathProblem.Absolute)
    else if segments.contains("..") then refuse(PathProblem.Traversal)
    else
      reserved match
        case Some(problem)                              => refuse(problem)
        case None if segments.contains(".")             => refuse(PathProblem.CurrentSegment)
        case None if segments.exists(_.isEmpty)         => refuse(PathProblem.EmptySegment)
        case None if segments.exists(_.startsWith(".")) =>
          refuse(PathProblem.Hidden(segments.filter(_.startsWith(".")).head))
        case None =>
          BundleArea.named(segments.head) match
            case None => refuse(PathProblem.OutsideAreas(segments.head))
            case Some(area) if segments.size == 1 => refuse(PathProblem.NoName(area))
            case Some(area)                       => Right(new BundlePath(value, area))

  /** The path `rest` inside `area`. */
  def in(area: BundleArea, rest: String): Either[BundleError, BundlePath] =
    of(s"${area.directory}/$rest")

  private def isDrive(value: String): Boolean =
    value.length >= 2 && value.charAt(1) == ':' && value.charAt(0).isLetter
