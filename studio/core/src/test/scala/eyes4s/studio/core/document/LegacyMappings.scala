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

import io.circe.Json

/** Documents as S5.2 saved them, before the phase was a required role
  * (S5.3): every column mapping without its phase binding.
  */
object LegacyMappings:

  /** The S5.2-era stored form of a mapping without a phase column. */
  val mappingJson: String =
    """[{"role":{"Participant":{}},"column":"participant"},""" +
      """{"role":{"Trial":{}},"column":"trial"},""" +
      """{"role":{"Occurrence":{}},"column":"occurrence"},""" +
      """{"role":{"Ordinal":{}},"column":"ordinal"},""" +
      """{"role":{"SampleCount":{}},"column":"sample_count"},""" +
      """{"role":{"X":{}},"column":"x"},{"role":{"Y":{}},"column":"y"},""" +
      """{"role":{"Onset":{}},"column":"onset_ms"},""" +
      """{"role":{"Duration":{}},"column":"duration_ms"}]"""

  private def isPhaseBinding(j: Json): Boolean =
    j.asObject.exists(o =>
      o.contains("column") && o("role").flatMap(_.asObject).exists(_.contains("Phase"))
    )

  /** `json` with every phase binding of every mapping removed, and every
    * inventory mapping (S5.2 recorded none).
    */
  def withoutPhase(json: Json): Json =
    json.arrayOrObject(
      json,
      items => Json.fromValues(items.filterNot(isPhaseBinding).map(withoutPhase)),
      obj =>
        // A dataset revision's inventory mapping, not an admission's binding.
        val spec = if obj.contains("sources") then obj.remove("inventory") else obj
        Json.fromJsonObject(spec.mapValues(withoutPhase))
    )

  /** `document` as S5.2 would have saved it: re-encoded without phases. */
  def legacy(document: StudioDocument): Either[String, StudioDocument] =
    StudioDocument
      .encode(document)
      .flatMap(j => StudioDocument.decode(withoutPhase(j)))
      .left
      .map(_.message)
