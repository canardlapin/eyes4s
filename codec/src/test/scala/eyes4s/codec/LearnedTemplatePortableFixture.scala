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

// Identical JSON values emitted by JVM and Scala.js with typed geometry hashes.
// format: off
object LearnedTemplatePortableFixture:
  val versionOne = """{"schema":{"name":"test.learned-template","version":1},"value":{"method":"eyes4s.training-mean-cosine-response/1","splitUnit":"participant","responseUnit":"score","heldOutGroups":["p4"],"trainingHash":"b78b0909fa358697","rows":[{"key":{"schema":{"name":"test.key","version":1},"value":"a"},"splitGroup":"p1","matchGroup":"a","response":2.0,"frame":{"id":"template","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":1.0,"yAxis":"Down"},"grid":{"id":"template@2x1","nx":2,"ny":1},"values":[1.0,0.0],"provenance":{"inputs":"6926124a7b1433c4","steps":[]}},{"key":{"schema":{"name":"test.key","version":1},"value":"b"},"splitGroup":"p2","matchGroup":"b","response":2.0,"frame":{"id":"template","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":1.0,"yAxis":"Down"},"grid":{"id":"template@2x1","nx":2,"ny":1},"values":[1.0,0.0],"provenance":{"inputs":"c61667659be21227","steps":[]}},{"key":{"schema":{"name":"test.key","version":1},"value":"c"},"splitGroup":"p3","matchGroup":"c","response":1.0,"frame":{"id":"template","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":1.0,"yAxis":"Down"},"grid":{"id":"template@2x1","nx":2,"ny":1},"values":[0.0,1.0],"provenance":{"inputs":"a71ba05c90f2c806","steps":[]}},{"key":{"schema":{"name":"test.key","version":1},"value":"excluded"},"splitGroup":"p1","matchGroup":"held","response":900.0,"frame":{"id":"template","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":1.0,"yAxis":"Down"},"grid":{"id":"template@2x1","nx":2,"ny":1},"values":[0.0,1.0],"provenance":{"inputs":"656e798acfb777a7","steps":[]}},{"key":{"schema":{"name":"test.key","version":1},"value":"held"},"splitGroup":"p4","matchGroup":"held","response":7.0,"frame":{"id":"template","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":1.0,"yAxis":"Down"},"grid":{"id":"template@2x1","nx":2,"ny":1},"values":[0.5,0.5],"provenance":{"inputs":"eb92544e9f575400","steps":[]}}]}}"""
// format: on
