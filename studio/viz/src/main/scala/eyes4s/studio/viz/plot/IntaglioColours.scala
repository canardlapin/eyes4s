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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.tokens.{Colour, StageToken, StageVariant, Theme, ThemedToken, Tokens}
import intaglio.Rgba

/** Studio tokens as Intaglio colours: the only way a studio scene gets a colour.
  *
  * A [[eyes4s.studio.app.tokens.Colour]] can be minted only in the token
  * source, so every Intaglio colour a studio scene draws traces to a token
  * (DESIGN_SPEC section 4).
  */
object IntaglioColours:

  /** The Intaglio colour of a token colour. */
  def toIntaglio(colour: Colour): Rgba =
    // A Colour's channels lie in 0..255 and its alpha in 0..100 percent by
    // construction, so Intaglio's range checks cannot fail here.
    Rgba.unsafe(colour.red, colour.green, colour.blue, colour.alphaPercent / 100.0)

  /** A theme token as an Intaglio colour. */
  def themed(theme: Theme, token: ThemedToken): Rgba = toIntaglio(Tokens.themed(theme, token))

  /** A stage token as an Intaglio colour. */
  def staged(stage: StageVariant, token: StageToken): Rgba = toIntaglio(
    Tokens.staged(stage, token)
  )
