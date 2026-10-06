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

package eyes4s.audittool.fixtures

import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving.Mirror

object References:
  private inline def fields[C]: Int = summonFrom {
    case product: Mirror.ProductOf[C] => product.fromProduct(EmptyTuple).asInstanceOf[Product].productArity
  }
  private inline def cases[T <: Tuple]: List[() => Int] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (head *: tail) => (() => fields[head]) :: cases[tail]
  private inline def derived[E](using mirror: Mirror.SumOf[E]): List[() => Int] =
    cases[mirror.MirroredElemTypes]
  val errors: List[() => Int] = derived[DeclaredError]

  def typedCase(error: DeclaredError.InvalidName): String = error.name

  def invalid(error: DeclaredError): String = error match
    case DeclaredError.InvalidName(name) => name
    case DeclaredError.Missing           => "missing"

  def value(item: DeclaredValue): String = item.value
