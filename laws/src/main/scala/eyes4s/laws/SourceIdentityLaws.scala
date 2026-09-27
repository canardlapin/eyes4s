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

package eyes4s.laws

import eyes4s.plan.*
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Published exact identity laws. The caller supplies independent witnesses
  * and the component set it deliberately changed; expected causes are not
  * inferred by calling the comparison under test.
  */
object SourceIdentityLaws extends Laws:
  def identity(
      declared: Gen[SourceRef],
      changes: Gen[(SourceRef, SourceRef, Set[IdentityChange])]
  ): RuleSet =
    new DefaultRuleSet(
      "sourceIdentity",
      None,
      "display names do not alter declared identity" -> forAll(declared) { source =>
        source.identity.nonEmpty && source
          .copy(label = source.label + "/moved")
          .identity == source.identity
      },
      "identical declarations classify by bytes" -> forAll(declared) { source =>
        SourceComparison.of(true, source, source) == SourceComparison.SameBytes &&
        SourceComparison.of(false, source, source) == SourceComparison.SameIdentity
      },
      "all and only changed components are reported even with identical bytes" -> forAll(
        changes
      ) { case (before, after, expected) =>
        val exact = SourceComparison.of(true, before, after) match
          case SourceComparison.ChangedIdentity(causes) =>
            causes.values == expected && expected.nonEmpty
          case _ => false
        Prop(exact && before.identity != after.identity)
      },
      "legacy is never classified as unchanged" -> forAll(declared) { source =>
        SourceComparison.of(
          true,
          source,
          source.copy(interpretation = SourceInterpretation.LegacyUnspecified)
        ) match
          case SourceComparison.ChangedIdentity(causes) =>
            causes.values == Set(IdentityChange.Undeclared)
          case _ => false
      }
    )

  /** Witnesses differ in one declared option and carry the same source records. */
  def options(witnesses: Gen[(SourceRef, SourceRef)]): RuleSet =
    new DefaultRuleSet(
      "sourceOptions",
      None,
      "every changed admission option changes source identity" -> forAll(witnesses) { (a, b) =>
        a.records == b.records && a.identity.nonEmpty && b.identity.nonEmpty && a.identity != b.identity &&
        SourceComparison.of(true, a, b) == SourceComparison.ChangedIdentity(
          IdentityChanges.of(IdentityChange.Options)
        )
      }
    )
