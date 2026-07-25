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

import eyes4s.kernel.*

import cats.kernel.laws.discipline.{CommutativeGroupTests, OrderTests}

/** Runs the published rule sets against the kernel's own instances.
  *
  * This suite is deliberately thin: it is the suite a downstream author copies
  * when they write their own `Warp` or their own `Smoother`, so it should show
  * nothing but `checkAll` calls and the tolerance being passed.
  */
class KernelLawsSuite extends munit.DisciplineSuite:

  import Generators.given

  // Warp: a partial category, its negative half, inverses, and the projection.
  checkAll("Warp.partialCategory", WarpLaws.category(Tolerance.exactish))
  checkAll("Warp.frameCheck", WarpLaws.compositionRefusesMismatch)
  checkAll("Warp.inverse", WarpLaws.inverses(Tolerance.exactish))
  checkAll("Warp.tangent", WarpLaws.tangentRoundTrip(Tolerance.roundTrip))

  // Time: the algebraic structure claimed in Span's scaladoc, tested by the
  // standard cats bundles rather than by hand-rolled assertions.
  checkAll("Span.commutativeGroup", CommutativeGroupTests[Span].commutativeGroup)
  checkAll("Span.order", OrderTests[Span].order)
  checkAll("Instant.order", OrderTests[Instant].order)
  checkAll("Angle.order", OrderTests[Angle].order)

end KernelLawsSuite
