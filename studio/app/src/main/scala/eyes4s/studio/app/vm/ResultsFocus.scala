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

package eyes4s.studio.app.vm

import eyes4s.studio.app.compare.ParticipantsNavigatorVM
import eyes4s.studio.app.layout.PaneDecl

/** Results controls and composite names come from served, portable values. */
object ResultsFocus:
  def pane(
      decl: PaneDecl,
      participants: ParticipantsNavigatorVM,
      content: Vector[FocusStop]
  ): PaneFocus =
    decl.id.value match
      case "compare.participants" =>
        val name = (Vector(decl.accessibleName) ++ participants.cursorText).mkString(", ")
        PaneFocus(Some(FocusStop(A11yRole.List, name)), Vector.empty)
      case _ => PaneFocus.served(decl, content)
