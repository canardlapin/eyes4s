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

package eyes4s.codec

import eyes4s.design.*
import eyes4s.plan.DefinitionId

/** Native fixed-feature recipe, explicitly distinct from the historical R recipe.
  * The caller must allocate a different schema identity/version from the legacy codec.
  * Reopening restores inputs and refits deterministically; no unverified coefficients
  * are accepted as a native solver output.
  */
object NativeTemplateRecipeCodec:
  def of[K: KeyDigest](
      schema: DefinitionId,
      keys: VersionedCodec[K]
  ): VersionedCodec[TemplateSplit[K]] =
    TemplateRecipeCodec.build(schema, keys, FittedTemplate.nativeMethod, native = true)
