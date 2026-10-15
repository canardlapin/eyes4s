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

// format: off
private[codec] object ScanpathMethodFixtures:
  val multiMatch: String = """{"schema":{"name":"eyes4s.scanpath-method","version":1},"value":{"unit":"px","method":{"name":"eyes4s.scanpath-multimatch","version":1},"parameters":{"cardinality":"BaselineThreeFixations","maximumAlignmentCells":"1234567","alignmentConvention":"CanonicalOperandsExactCostTracebackV1"}}}"""
  val overlap: String = """{"schema":{"name":"eyes4s.scanpath-method","version":1},"value":{"unit":"px","method":{"name":"eyes4s.fixation-overlap","version":1},"parameters":{"clock":"exact queries","queries":["9223372036854775807","-9223372036854775808","9007199254740993","9223372036854775807"],"threshold":3.5,"distance":"Manhattan","endpoint":"OnsetRange","missing":"RequireComplete"}}}"""
  val transport: String = """{"schema":{"name":"eyes4s.scanpath-method","version":1},"value":{"unit":"px","method":{"name":"eyes4s.fixation-transport","version":1},"parameters":{"xScale":12.5,"yScale":2.25,"timeScaleMicros":"9007199254740993","timeWeight":0.25,"lambda":0.125,"maximumIterations":321,"marginalTolerance":1.0E-8,"maximumCosts":1234,"maximumWork":"9223372036854775807"}}}"""
// format: on
