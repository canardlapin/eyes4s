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

import eyes4s.plan.DefinitionId

/** Studio's schema identities are its own: public `DefinitionId.of`
  * identities in the `studio.` namespace, pinned, valid and distinct.
  */
class StudioSchemaIdsSuite extends munit.FunSuite:

  test("the studio schema ids are pinned") {
    assertEquals(
      StudioSchemaIds.ids.map(_.all.map(id => (id.name, id.version))),
      Right(
        Vector(
          ("studio.document", 1),
          ("studio.science", 1),
          ("studio.journal", 1),
          ("studio.dataset-content", 1),
          ("studio.project", 1),
          ("studio.asset-registry", 1),
          ("studio.run-archive", 1),
          ("studio.reporting-spec", 1)
        )
      )
    )
  }

  test("every studio schema id is valid under DefinitionId.of, distinct and namespaced") {
    val ids = StudioSchemaIds.ids.toOption.get.all
    ids.foreach(id => assertEquals(DefinitionId.of(id.name, id.version), Right(id)))
    assertEquals(ids.distinct.size, ids.size)
    assertEquals(ids.map(_.name).distinct.size, ids.size)
    assert(ids.forall(_.name.startsWith("studio.")), ids)
    assert(ids.forall(!_.name.startsWith("eyes4s.")), ids)
  }

  test("the document and science codecs are built on those ids") {
    val ids = StudioSchemaIds.ids.toOption.get
    // The document ladder starts at its id; version 2 (S5.4), version 3
    // (S5.6), version 4 (S5.7), version 5 (S7.1), and version 6
    // (explicit reporting contrast operands), version 7 (initial drafts), and version 8 (stored methods) are its next rungs.
    assertEquals(StudioDocument.ladder.map(_.versions.head), Right(ids.document))
    assertEquals(
      StudioDocument.codec.map(c => (c.schema.name, c.schema.version)),
      Right((ids.document.name, ids.document.version + 10))
    )
    assertEquals(ScienceContent.ladder.map(_.versions.head), Right(ids.science))
    assertEquals(
      ScienceContent.codec.map(c => (c.schema.name, c.schema.version)),
      Right((ids.science.name, ids.science.version + 6))
    )
  }
