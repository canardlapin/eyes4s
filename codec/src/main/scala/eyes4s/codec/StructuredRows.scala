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

import io.circe.Json

/** Restore a described array without retaining an intermediate Vector[Json]. */
private[codec] object StructuredRows:
  def size(document: CanonicalDoc, path: String): Either[CodecError, Int] =
    array(document, path).map(_._1)

  def describeMap(document: CanonicalDoc, path: String)(
      transform: (CanonicalDoc, Int) => Either[CodecError, CanonicalDoc]
  ): Either[CodecError, CanonicalDoc] =
    array(document, path).map { (size, item) =>
      CanonicalDoc.Arr(size, i => item(i).flatMap(value => transform(value, i)))
    }

  private def array(
      document: CanonicalDoc,
      path: String
  ): Either[CodecError, (Int, Int => Either[CodecError, CanonicalDoc])] = document match
    case CanonicalDoc.Deferred(make)  => array(make(), path)
    case CanonicalDoc.Arr(size, item) =>
      if size < 0 then Left(CodecError.Unsupported(path, s"negative array size $size"))
      else Right((size, item))
    case CanonicalDoc.Leaf(json) =>
      json.asArray
        .map(a => (a.size, (i: Int) => Right(CanonicalDoc.Leaf(a(i)))))
        .toRight(CodecError.Field(path, json, "expected an array"))
    case _: CanonicalDoc.Obj =>
      Left(CodecError.Unsupported(path, "expected an array, found an object"))

  def read[A](document: CanonicalDoc, path: String)(
      row: (Json, Int) => Either[CodecError, A]
  ): Either[CodecError, Vector[A]] =
    readDocument(document, path)((doc, i) => doc.json.flatMap(j => row(j, i)))

  def readDocument[A](document: CanonicalDoc, path: String)(
      row: (CanonicalDoc, Int) => Either[CodecError, A]
  ): Either[CodecError, Vector[A]] =
    array(document, path).flatMap { (size, item) =>
      val out     = Vector.newBuilder[A]
      var i       = 0
      var failure = Option.empty[CodecError]
      while i < size && failure.isEmpty do
        item(i).flatMap(doc => row(doc, i)) match
          case Right(value) => out += value
          case Left(error)  => failure = Some(Wire.at(s"$path[$i]")(error))
        i += 1
      failure.toLeft(out.result())
    }
