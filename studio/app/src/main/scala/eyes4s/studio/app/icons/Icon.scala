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

package eyes4s.studio.app.icons

/** How one layer of an icon is painted. Every layer is painted in the current
  * colour, the colour of the control that shows the icon; an icon names no
  * colour of its own (DESIGN_SPEC section 4).
  */
enum IconPaint derives CanEqual:

  /** Outlined: a [[Icon.strokeWidth]] stroke, no fill. Butt caps and mitre
    * joins, as in SVG.
    */
  case Stroke

  /** Filled with the non-zero winding rule. */
  case Fill

  /** Filled with the even-odd rule, so an inner subpath cuts a hole (the
    * highlight of the eye-aperture pupil).
    */
  case FillEvenOdd

/** One path of an icon: SVG path data in the 16x16 view box. */
final case class IconLayer(pathData: String, paint: IconPaint)

/** Where an icon's drawing comes from. */
enum IconSource derives CanEqual:

  /** Copied from a design board, rescaled to the 16x16 grid where the board
    * drew it on another grid.
    */
  case Board(file: String)

  /** Drawn for the studio in the boards' style: the boards show the concept
    * but no glyph for it, or draw it with text or a fill pattern.
    */
  case Drawn

/** The studio icon set (ticket S1.3; board System.dc.html).
  *
  * Every icon is drawn in a 16x16 view box with a 1.5 stroke, in the current
  * colour, as the boards draw theirs. `id` is the resource name: studio-desktop
  * gives a node the style classes `icon` and `icon-<id>`. The first
  * twenty-two are the ticket's list; the rest are the other glyphs the boards
  * draw.
  */
enum Icon(val id: String, val layers: List[IconLayer], val source: IconSource) derives CanEqual:

  // --- Chrome -------------------------------------------------------------

  /** Opens a menu (project chip, drop-downs). Board grid 10, scaled by 1.6. */
  case ChevronDown
      extends Icon(
        "chevron-down",
        List(IconLayer("M3.2 5.6 8 10.4 12.8 5.6", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

  /** A collapsed tree row. Board grid 10, scaled by 1.6. */
  case ChevronRight
      extends Icon(
        "chevron-right",
        List(IconLayer("M5.6 3.2 10.4 8 5.6 12.8", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

  /** Trail back (⌘[). */
  case Back
      extends Icon(
        "back",
        List(IconLayer("M10 3 5 8l5 5", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

  /** Trail forward (⌘]). */
  case Forward
      extends Icon(
        "forward",
        List(IconLayer("m6 3 5 5-5 5", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

  /** Minimize a dock group. */
  case Minimize
      extends Icon(
        "minimize",
        List(IconLayer("M4 8h8", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

  /** Maximize a dock group. */
  case Maximize
      extends Icon(
        "maximize",
        List(IconLayer("M3 3h10v10H3Z", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

  /** Pop a pane out into its own window. */
  case PopOut
      extends Icon(
        "pop-out",
        List(IconLayer("M9 3h4v4M13 3 7.5 8.5M11 10v3H3V5h3", IconPaint.Stroke)),
        IconSource.Board("Explore.dc.html")
      )

  /** Sort a table column. */
  case Sort
      extends Icon(
        "sort",
        List(
          IconLayer(
            "M5 3v10M2.5 10.5 5 13l2.5-2.5M11 13V3M8.5 5.5 11 3l2.5 2.5",
            IconPaint.Stroke
          )
        ),
        IconSource.Board("Main.dc.html")
      )

  /** The Jobs chip, idle. */
  case Jobs
      extends Icon(
        "jobs",
        List(IconLayer("M3 4h10M3 8h10M3 12h6", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

  /** Search a list or table. */
  case Search
      extends Icon(
        "search",
        List(
          IconLayer(
            "M2.75 7a4.25 4.25 0 1 0 8.5 0a4.25 4.25 0 1 0-8.5 0ZM10.1 10.1 13.75 13.75",
            IconPaint.Stroke
          )
        ),
        IconSource.Drawn
      )

  /** Local-only data, and a locked figure binding. */
  case Lock
      extends Icon(
        "lock",
        List(IconLayer("M3 7h10v7H3ZM5.5 7V5a2.5 2.5 0 0 1 5 0v2", IconPaint.Stroke)),
        IconSource.Board("DataEmpty.dc.html")
      )

  /** A warning diagnostic. */
  case Warning
      extends Icon(
        "warning",
        List(IconLayer("M8 2 14.5 13.5h-13ZM8 6.25v3.5M8 11v1.25", IconPaint.Stroke)),
        IconSource.Drawn
      )

  // --- Display kinds: what was on screen (System board, card 07) ----------

  /** A stimulus image was displayed. */
  case DisplayImage
      extends Icon(
        "display-image",
        List(
          IconLayer("M1.75 3.25h12.5v9.5H1.75ZM3 11.5 7 7l3 3 2-2 2.25 3", IconPaint.Stroke)
        ),
        IconSource.Board("Data.dc.html")
      )

  /** A blank screen was displayed. */
  case DisplayBlank
      extends Icon(
        "display-blank",
        List(IconLayer("M1.75 3.25h12.5v9.5H1.75Z", IconPaint.Stroke)),
        IconSource.Board("System.dc.html")
      )

  /** A blank screen with a fixation cross. */
  case DisplayCross
      extends Icon(
        "display-cross",
        List(IconLayer("M1.75 3.25h12.5v9.5H1.75ZM8 5.75v4.5M5.75 8h4.5", IconPaint.Stroke)),
        IconSource.Board("Explore.dc.html")
      )

  /** A verbal cue. The board prints the cue word; the glyph draws two lines
    * of text.
    */
  case DisplayCue
      extends Icon(
        "display-cue",
        List(IconLayer("M1.75 3.25h12.5v9.5H1.75ZM5 7h6M5 9.5h4", IconPaint.Stroke)),
        IconSource.Drawn
      )

  /** The display is not known. A dashed screen, as on the board, with a
    * question mark drawn as a path.
    */
  case DisplayUnknown
      extends Icon(
        "display-unknown",
        List(
          IconLayer(
            "M1.75 3.25h2M5.25 3.25h2M8.75 3.25h2M12.25 3.25h2" +
              "M1.75 12.75h2M5.25 12.75h2M8.75 12.75h2M12.25 12.75h2" +
              "M1.75 3.25v2M1.75 7v2M1.75 10.75v2M14.25 3.25v2M14.25 7v2M14.25 10.75v2" +
              "M6.25 6.5a1.75 1.75 0 1 1 2.6 1.5c-.55.3-.85.65-.85 1.25v.5M8 10.25v1",
            IconPaint.Stroke
          )
        ),
        IconSource.Drawn
      )

  /** The displayed asset is missing: never drawn as blank. */
  case DisplayMissing
      extends Icon(
        "display-missing",
        List(
          IconLayer(
            "M1.75 3.25h12.5v9.5H1.75ZM5.5 5.5 10.5 10.5M10.5 5.5 5.5 10.5",
            IconPaint.Stroke
          )
        ),
        IconSource.Board("System.dc.html")
      )

  // --- Roles: hue + shape + lightness (DESIGN_SPEC section 5) -------------

  /** The query: a filled circle. */
  case RoleQuery
      extends Icon(
        "role-query",
        List(IconLayer("M3 8a5 5 0 1 0 10 0a5 5 0 1 0-10 0Z", IconPaint.Fill)),
        IconSource.Board("System.dc.html")
      )

  /** The matched reference: a filled diamond. */
  case RoleMatched
      extends Icon(
        "role-matched",
        List(IconLayer("M8 2.25 13.75 8 8 13.75 2.25 8Z", IconPaint.Fill)),
        IconSource.Board("System.dc.html")
      )

  /** A control: a hollow circle. */
  case RoleControl
      extends Icon(
        "role-control",
        List(
          IconLayer("M3.25 8a4.75 4.75 0 1 0 9.5 0a4.75 4.75 0 1 0-9.5 0Z", IconPaint.Stroke)
        ),
        IconSource.Board("System.dc.html")
      )

  /** The eye-aperture mark of the app bar: the board's 20-unit mark scaled by
    * 0.8. The pupil's highlight is a hole, not a surface-coloured dot.
    */
  case EyeAperture
      extends Icon(
        "eye-aperture",
        List(
          IconLayer(
            "M1.2 8C3.2 4.4 5.6 2.8 8 2.8S12.8 4.4 14.8 8C12.8 11.6 10.4 13.2 8 13.2S3.2 11.6 1.2 8Z",
            IconPaint.Stroke
          ),
          IconLayer(
            "M5.44 8a2.56 2.56 0 1 0 5.12 0a2.56 2.56 0 1 0-5.12 0Z" +
              "M8.16 7.04a.8 .8 0 1 0 1.6 0a.8 .8 0 1 0-1.6 0Z",
            IconPaint.FillEvenOdd
          )
        ),
        IconSource.Board("System.dc.html")
      )

  // --- Other glyphs the boards draw ---------------------------------------

  /** The Jobs chip while a run is in progress. */
  case JobsRunning
      extends Icon(
        "jobs-running",
        List(IconLayer("M8 2a6 6 0 1 1-6 6", IconPaint.Stroke)),
        IconSource.Board("Results.dc.html")
      )

  /** The Jobs chip after a run failed. Board grid 10, scaled by 1.6. */
  case JobsFailed
      extends Icon(
        "jobs-failed",
        List(IconLayer("M3.2 3.2 12.8 12.8M12.8 3.2 3.2 12.8", IconPaint.Stroke)),
        IconSource.Board("System.dc.html")
      )

  /** Compare two analysis revisions. */
  case CompareRevisions
      extends Icon(
        "compare-revisions",
        List(IconLayer("M5 2v12M11 2v12M2 5h6M8 11h6", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

  /** A folder source (stimulus images). */
  case Folder
      extends Icon(
        "folder",
        List(IconLayer("M1.5 4h5l1.5 1.5h6.5v8h-13z", IconPaint.Stroke)),
        IconSource.Board("DataEmpty.dc.html")
      )

  /** A file source (fixations.csv). */
  case File
      extends Icon(
        "file",
        List(
          IconLayer("M3.5 1.5h6l3 3v10h-9z", IconPaint.Stroke),
          IconLayer("M9.5 1.5v3h3", IconPaint.Stroke)
        ),
        IconSource.Board("DataEmpty.dc.html")
      )

  /** Timeline play. */
  case Play
      extends Icon(
        "play",
        List(IconLayer("M4 3v10l9-5z", IconPaint.Fill)),
        IconSource.Board("Explore.dc.html")
      )

  /** Timeline step back. */
  case StepBack
      extends Icon(
        "step-back",
        List(IconLayer("M4 3v10M13 3 6 8l7 5z", IconPaint.Stroke)),
        IconSource.Board("Explore.dc.html")
      )

  /** Timeline step forward. */
  case StepForward
      extends Icon(
        "step-forward",
        List(IconLayer("M12 3v10M3 3l7 5-7 5z", IconPaint.Stroke)),
        IconSource.Board("Explore.dc.html")
      )

  /** A passed preflight check. Board grid 12, scaled by 4/3. */
  case Check
      extends Icon(
        "check",
        List(IconLayer("M3.33 8.67 6.67 12l6-8", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

  /** Freshness: current (the teal dot). Board grid 12, scaled by 4/3. */
  case FreshnessCurrent
      extends Icon(
        "freshness-current",
        List(
          IconLayer("M3.33 8a4.67 4.67 0 1 0 9.34 0a4.67 4.67 0 1 0-9.34 0Z", IconPaint.Fill)
        ),
        IconSource.Board("Analysis.dc.html")
      )

  /** Freshness: a draft, not yet run. The board's dashed square as dashes. */
  case FreshnessDraft
      extends Icon(
        "freshness-draft",
        List(
          IconLayer(
            "M2 2h2.5M6.75 2h2.5M11.5 2H14M14 2v2.5M14 6.75v2.5M14 11.5V14" +
              "M14 14h-2.5M9.25 14h-2.5M4.5 14H2M2 14v-2.5M2 9.25v-2.5M2 4.5V2",
            IconPaint.Stroke
          )
        ),
        IconSource.Board("Analysis.dc.html")
      )

  /** Freshness: a cancelled run. Board grid 12, scaled by 4/3. */
  case FreshnessCancelled
      extends Icon(
        "freshness-cancelled",
        List(
          IconLayer(
            "M2.67 8a5.33 5.33 0 1 0 10.66 0a5.33 5.33 0 1 0-10.66 0ZM4.67 11.33 11.33 4.67",
            IconPaint.Stroke
          )
        ),
        IconSource.Board("Analysis.dc.html")
      )

  /** Freshness: stale, run on older data. The board's hatch fill as three
    * strokes.
    */
  case FreshnessStale
      extends Icon(
        "freshness-stale",
        List(IconLayer("M2 2h12v12H2ZM2 7.5 8.5 14M2 2l12 12M7.5 2 14 8.5", IconPaint.Stroke)),
        IconSource.Board("Analysis.dc.html")
      )

object Icon:

  /** The view box: every icon is drawn in `0 0 16 16`. */
  val viewBox: Int = 16

  /** The stroke width of outlined layers, in view-box units. */
  val strokeWidth: Double = 1.5

  /** The icon with resource name `id`. */
  def byId(id: String): Option[Icon] = values.find(_.id == id)

/** The text a screen reader announces for an icon-only control. It is never
  * blank: an icon-only button without it is unusable without sight
  * (DESIGN_SPEC section 10).
  */
opaque type AccessibleText = String

object AccessibleText:

  /** `text`, trimmed, when it is not blank. */
  def parse(text: String): Either[AccessibleTextError, AccessibleText] =
    val trimmed = text.trim
    if trimmed.isEmpty then Left(AccessibleTextError.Blank(text)) else Right(trimmed)

  extension (a: AccessibleText) def text: String = a

/** Why a string cannot be an icon-only control's accessible text. */
enum AccessibleTextError derives CanEqual:
  case Blank(text: String)

  def message: String = this match
    case Blank(t) => s"accessible text '$t' is blank; an icon-only control needs a name"
