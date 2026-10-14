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

package eyes4s.io

import scala.scalajs.js

private[io] object PublicAscFixture:
  def bytes: IArray[Byte] =
    val fs        = js.Dynamic.global.require("fs")
    val path      = js.Dynamic.global.require("path")
    val relative  = "io/src/test/resources/eyes4s/io/eyelink/public/zenodo-1156863/C023.asc"
    var directory = js.Dynamic.global.process.cwd().asInstanceOf[String]
    var candidate = path.join(directory, relative).asInstanceOf[String]
    while !fs.existsSync(candidate).asInstanceOf[Boolean] &&
      path.dirname(directory).asInstanceOf[String] != directory
    do
      directory = path.dirname(directory).asInstanceOf[String]
      candidate = path.join(directory, relative).asInstanceOf[String]
    val buffer = fs.readFileSync(candidate)
    IArray.from(Array.tabulate(buffer.length.asInstanceOf[Int]) { index =>
      buffer.selectDynamic(index.toString).asInstanceOf[Int].toByte
    })
