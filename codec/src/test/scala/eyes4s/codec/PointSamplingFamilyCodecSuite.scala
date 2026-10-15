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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.pointcodecconsumer.PointSamplingFixture
import io.circe.Json

class PointSamplingFamilyCodecSuite extends munit.FunSuite:
  private def get[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)
  private def id(value: String)             = get(DefinitionId.of(value, 1))
  private val layout                        = StudyKey.layout(id("example.point-layout"))
  private val keys                          = StudyCodecs.key(id("example.point-key"))
  private val legacy                        = new PointSamplingCodec[StudyKey, Px](
    id("example.point-plan"),
    id("example.point-result"),
    layout,
    keys
  )
  private val family      = new PointSamplingFamilyCodec[StudyKey, Px](layout, keys)
  private val oldDocument = get(io.circe.parser.parse(PointSamplingFixture.versionOne))
  private def archive     = get(legacy.archive.decode(oldDocument))
  private def alter(document: Json, field: String, value: Json): Json =
    document.mapObject(
      _.add("value", get(document.hcursor.get[Json]("value")).mapObject(_.add(field, value)))
    )

  test(
    "built-in plan and stamped result round-trip while retaining the legacy numerical payload"
  ) {
    val a         = archive
    val completed = get(PointSamplingArchive.completed(a.plan, a.result))
    assert(completed.result eq a.result)
    val saved    = get(family.results.encode(completed))
    val restored = get(family.results.decode(saved))
    assertEquals(get(family.results.encode(restored)), saved)
    assertEquals(
      get(family.plans.encode(get(family.plans.decode(get(family.plans.encode(a.plan)))))),
      get(family.plans.encode(a.plan))
    )
    assertEquals(
      get(saved.hcursor.downField("value").get[Json]("result")),
      get(oldDocument.hcursor.downField("value").get[Json]("result"))
    )
    val claimed = get(saved.hcursor.downField("value").get[Json]("runStamp"))
    assertEquals(
      get(claimed.hcursor.get[String]("plan")),
      get(family.plans.digest(a.plan)).sha256.hex
    )
    assertEquals(
      get(claimed.hcursor.get[String]("input")),
      get(claimed.hcursor.get[String]("plan"))
    )
    assertEquals(get(legacy.archive.encode(a)), oldDocument)
  }

  test("missing and stale canonical stamps are refused before result replay") {
    val saved   = get(family.results.encode(archive))
    val payload = get(saved.hcursor.get[Json]("value"))
    val absent  = saved.mapObject(_.add("value", payload.mapObject(_.remove("runStamp"))))
    assert(family.results.decode(absent).isLeft)
    val wrong =
      Json.obj("plan" -> Json.fromString("0" * 64), "input" -> Json.fromString("0" * 64))
    val changed = alter(alter(saved, "runStamp", wrong), "result", Json.Null)
    family.results.decode(changed) match
      case Left(CodecError.Stamp(_)) => ()
      case other => fail(s"Expected stamp refusal before invalid result, got $other")
  }

  test("numeric payload tampering is refused by replay, even with a valid plan stamp") {
    val saved = get(family.results.encode(archive))
    assert(family.results.decode(alter(saved, "result", Json.Null)).isLeft)
  }

  test(
    "completed-result binding rejects a different recipe without re-executing either result"
  ) {
    val a       = archive; val s = a.plan.spec
    val changed = get(
      PointSamplingSpec.of(
        s.clock,
        Vector.empty,
        None,
        s.normalization,
        s.lookup,
        s.endpoint,
        s.controls,
        s.controlPolicy,
        s.binPolicy
      )
    )
    val other = PointSamplingPlan.of(layout, a.plan.source, a.plan.templates, changed)
    assert(PointSamplingArchive.completed(other, a.result).isLeft)
    val done = get(PointSamplingArchive.completed(a.plan, a.result))
    assert(done.result eq a.result)
  }

  test("generic analysis registration preserves embedded input identity and typed values") {
    val a = archive; val registry = get(AnalysisRegistry.empty.register(family.registration))
    val p = get(registry.plan(family.plans.schema, get(family.plans.encode(a.plan))))
    val result = get(registry.result(family.results.schema, get(family.results.encode(a))))
    assertEquals(p.embeddedInputs, Vector(a.plan.inputHash))
    assertEquals(result.inputs, Vector(a.plan.inputHash))
    assertEquals(p.description, a.plan.description)
    assertEquals(result.description, a.plan.description)
    assertEquals(
      LoadedAnalysisPlan.typed(family.plans)(p).map(_.description),
      Some(a.plan.description)
    )
    assert(LoadedAnalysisResult.typed(family.results)(result).nonEmpty)
    assert(registry.register(family.registration).isLeft)
  }

  test("generic manifest roles resolve a stamped result with its embedded plan input") {
    val a      = archive
    val plan   = get(StoredArtifact.analysisPlan("point-plan", family.plans, a.plan))
    val result = get(StoredArtifact.analysisResult("point-result", family.results, a))
    val graph  = get(
      SavedManifest.of(
        Vector(plan, result),
        Vector(
          ManifestRelation.AnalysisResultOf(
            result.name,
            plan.name,
            AnalysisInputs.EmbeddedInPlan
          )
        )
      )
    )
    val registry = get(AnalysisRegistry.empty.register(family.registration))
    val decoders = get(ArtifactDecoders.study[Px]).withAnalyses(registry)
    val resolved = get(ArtifactResolver.resolve(graph.address, graph.source, decoders))
    val loaded   =
      resolved.analysisResult(result.name).getOrElse(fail("No point-sampling result"))
    assertEquals(loaded.inputs, Vector(a.plan.inputHash))
    assertEquals(loaded.encode, family.results.encode(a))
    assert(LoadedAnalysisResult.typed(family.results)(loaded).nonEmpty)
  }

  // format: off
  private val pinnedPlan = """{"schema":{"name":"eyes4s.point-sampling-plan","version":1},"value":{"layout":{"name":"example.point-layout","version":1},"keySchema":{"name":"example.point-key","version":1},"method":"eyes4s.static-density-onset-sampling-arithmetic-mean/1","source":{"schema":{"name":"eyes4s.study-input","version":1},"value":{"layout":{"name":"example.point-layout","version":1},"keySchema":{"name":"example.point-key","version":1},"unit":"px","input":"5ea4ce0156141a04","identities":{"frames":[{"id":"point-codec","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"}],"grids":[],"clocks":["point-clock"]},"trials":{"schema":{"name":"eyes4s.trials","version":1},"value":[{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"A","phase":"source"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"point-codec","clock":"point-clock","fixations":[{"onsetMicros":"0","offsetMicros":"5000","x":0.5,"y":0.5,"sampleCount":1},{"onsetMicros":"10000","offsetMicros":"15000","x":1.5,"y":0.5,"sampleCount":1},{"onsetMicros":"20000","offsetMicros":"25000","x":1.5,"y":0.5,"sampleCount":1}]}}},{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"B","phase":"source"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"point-codec","clock":"point-clock","fixations":[{"onsetMicros":"0","offsetMicros":"5000","x":0.5,"y":0.5,"sampleCount":1},{"onsetMicros":"10000","offsetMicros":"15000","x":1.5,"y":0.5,"sampleCount":1},{"onsetMicros":"20000","offsetMicros":"25000","x":1.5,"y":0.5,"sampleCount":1}]}}},{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"source"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"point-codec","clock":"point-clock","fixations":[{"onsetMicros":"0","offsetMicros":"5000","x":0.5,"y":0.5,"sampleCount":1},{"onsetMicros":"10000","offsetMicros":"15000","x":1.5,"y":0.5,"sampleCount":1},{"onsetMicros":"20000","offsetMicros":"25000","x":1.5,"y":0.5,"sampleCount":1}]}}}]}}},"templates":[{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"A","phase":"template"}},"frame":{"id":"point-codec","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"},"grid":{"id":"point-codec@2x2","nx":2,"ny":2},"values":[1.0,2.0,4.0,8.0],"provenance":{"inputs":"8aa43319fd5bac4c","steps":[]}},{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"B","phase":"template"}},"frame":{"id":"point-codec","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"},"grid":{"id":"point-codec@2x2","nx":2,"ny":2},"values":[2.0,2.0,4.0,8.0],"provenance":{"inputs":"346a2ecdb3565fd1","steps":[]}},{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"template"}},"frame":{"id":"point-codec","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"},"grid":{"id":"point-codec@2x2","nx":2,"ny":2},"values":[3.0,2.0,4.0,8.0],"provenance":{"inputs":"650c455382d20c09","steps":[]}}],"clock":"point-clock","queries":["20000","0","10000","-1","20001"],"bins":{"clock":"point-clock","boundaries":["0","10000","20000"],"endpoint":"IncludeFinalEndpoint"},"normalization":"SampleZScore","lookup":"NearestClampedRIndex","trajectoryEndpoint":"OnsetRange","controls":{"kind":"candidates","selection":{"kind":"bottomK","cap":1,"seed":"-9223372036854775801","sampleId":"persisted-points"}},"controlPolicy":{"kind":"successfulOnly","minimum":1},"binPolicy":{"kind":"successfulOnly","minimum":1},"inputHash":"6b0d7f267940501e","planHash":"4069abc18c10519e"}}"""
  private val pinnedResult = """{"schema":{"name":"eyes4s.point-sampling-result","version":1},"value":{"runStamp":{"plan":"b76c6c58906dadf6cc259180df124fa17cbff145465e665f98b4122c4b5a4a45","input":"b76c6c58906dadf6cc259180df124fa17cbff145465e665f98b4122c4b5a4a45"},"plan":{"schema":{"name":"eyes4s.point-sampling-plan","version":1},"value":{"layout":{"name":"example.point-layout","version":1},"keySchema":{"name":"example.point-key","version":1},"method":"eyes4s.static-density-onset-sampling-arithmetic-mean/1","source":{"schema":{"name":"eyes4s.study-input","version":1},"value":{"layout":{"name":"example.point-layout","version":1},"keySchema":{"name":"example.point-key","version":1},"unit":"px","input":"5ea4ce0156141a04","identities":{"frames":[{"id":"point-codec","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"}],"grids":[],"clocks":["point-clock"]},"trials":{"schema":{"name":"eyes4s.trials","version":1},"value":[{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"A","phase":"source"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"point-codec","clock":"point-clock","fixations":[{"onsetMicros":"0","offsetMicros":"5000","x":0.5,"y":0.5,"sampleCount":1},{"onsetMicros":"10000","offsetMicros":"15000","x":1.5,"y":0.5,"sampleCount":1},{"onsetMicros":"20000","offsetMicros":"25000","x":1.5,"y":0.5,"sampleCount":1}]}}},{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"B","phase":"source"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"point-codec","clock":"point-clock","fixations":[{"onsetMicros":"0","offsetMicros":"5000","x":0.5,"y":0.5,"sampleCount":1},{"onsetMicros":"10000","offsetMicros":"15000","x":1.5,"y":0.5,"sampleCount":1},{"onsetMicros":"20000","offsetMicros":"25000","x":1.5,"y":0.5,"sampleCount":1}]}}},{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"source"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"point-codec","clock":"point-clock","fixations":[{"onsetMicros":"0","offsetMicros":"5000","x":0.5,"y":0.5,"sampleCount":1},{"onsetMicros":"10000","offsetMicros":"15000","x":1.5,"y":0.5,"sampleCount":1},{"onsetMicros":"20000","offsetMicros":"25000","x":1.5,"y":0.5,"sampleCount":1}]}}}]}}},"templates":[{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"A","phase":"template"}},"frame":{"id":"point-codec","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"},"grid":{"id":"point-codec@2x2","nx":2,"ny":2},"values":[1.0,2.0,4.0,8.0],"provenance":{"inputs":"8aa43319fd5bac4c","steps":[]}},{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"B","phase":"template"}},"frame":{"id":"point-codec","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"},"grid":{"id":"point-codec@2x2","nx":2,"ny":2},"values":[2.0,2.0,4.0,8.0],"provenance":{"inputs":"346a2ecdb3565fd1","steps":[]}},{"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"template"}},"frame":{"id":"point-codec","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"},"grid":{"id":"point-codec@2x2","nx":2,"ny":2},"values":[3.0,2.0,4.0,8.0],"provenance":{"inputs":"650c455382d20c09","steps":[]}}],"clock":"point-clock","queries":["20000","0","10000","-1","20001"],"bins":{"clock":"point-clock","boundaries":["0","10000","20000"],"endpoint":"IncludeFinalEndpoint"},"normalization":"SampleZScore","lookup":"NearestClampedRIndex","trajectoryEndpoint":"OnsetRange","controls":{"kind":"candidates","selection":{"kind":"bottomK","cap":1,"seed":"-9223372036854775801","sampleId":"persisted-points"}},"controlPolicy":{"kind":"successfulOnly","minimum":1},"binPolicy":{"kind":"successfulOnly","minimum":1},"inputHash":"6b0d7f267940501e","planHash":"4069abc18c10519e"}},"result":{"rows":[{"index":0,"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"A","phase":"source"}},"template":{"kind":"success","key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"A","phase":"template"}}},"eligibleControls":2,"selectedControls":1,"controls":[{"occurrence":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"source"}},"template":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"template"}},"values":[{"kind":"success","value":-0.8555277382080452},{"kind":"success","value":-0.4752931878933584},{"kind":"success","value":-0.8555277382080452},{"kind":"failure","error":{"kind":"trajectory","reason":"BeforeFirstOnset"}},{"kind":"failure","error":{"kind":"trajectory","reason":"AfterLastOnset"}}]}],"points":[{"index":0,"timeMicros":"20000","point":[1.5,0.5],"value":{"kind":"success","value":-0.5653009971610737},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.8555277382080452}},"bin":1},{"index":1,"timeMicros":"0","point":[0.5,0.5],"value":{"kind":"success","value":-0.8883301383959731},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.4752931878933584}},"bin":0},{"index":2,"timeMicros":"10000","point":[1.5,0.5],"value":{"kind":"success","value":-0.5653009971610737},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.8555277382080452}},"bin":1},{"index":3,"timeMicros":"-1","point":null,"value":{"kind":"failure","error":{"kind":"trajectory","reason":"BeforeFirstOnset"}},"control":{"requested":1,"successful":0,"contributing":0,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"failure","error":{"kind":"insufficient","level":"query[3] controls","requested":1,"successful":0,"minimum":1}}},"bin":null},{"index":4,"timeMicros":"20001","point":null,"value":{"kind":"failure","error":{"kind":"trajectory","reason":"AfterLastOnset"}},"control":{"requested":1,"successful":0,"contributing":0,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"failure","error":{"kind":"insufficient","level":"query[4] controls","requested":1,"successful":0,"minimum":1}}},"bin":null}],"bins":[{"index":0,"fromMicros":"0","untilMicros":"10000","includesFinalEndpoint":false,"queries":[1],"observed":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.8883301383959731}},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.4752931878933584}},"difference":{"kind":"success","value":-0.4130369505026147}},{"index":1,"fromMicros":"10000","untilMicros":"20000","includesFinalEndpoint":true,"queries":[0,2],"observed":{"requested":2,"successful":2,"contributing":2,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.5653009971610737}},"control":{"requested":2,"successful":2,"contributing":2,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.8555277382080452}},"difference":{"kind":"success","value":0.29022674104697144}}]},{"index":1,"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"B","phase":"source"}},"template":{"kind":"success","key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"B","phase":"template"}}},"eligibleControls":2,"selectedControls":1,"controls":[{"occurrence":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"source"}},"template":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"template"}},"values":[{"kind":"success","value":-0.8555277382080452},{"kind":"success","value":-0.4752931878933584},{"kind":"success","value":-0.8555277382080452},{"kind":"failure","error":{"kind":"trajectory","reason":"BeforeFirstOnset"}},{"kind":"failure","error":{"kind":"trajectory","reason":"AfterLastOnset"}}]}],"points":[{"index":0,"timeMicros":"20000","point":[1.5,0.5],"value":{"kind":"success","value":-0.7071067811865475},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.8555277382080452}},"bin":1},{"index":1,"timeMicros":"0","point":[0.5,0.5],"value":{"kind":"success","value":-0.7071067811865475},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.4752931878933584}},"bin":0},{"index":2,"timeMicros":"10000","point":[1.5,0.5],"value":{"kind":"success","value":-0.7071067811865475},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.8555277382080452}},"bin":1},{"index":3,"timeMicros":"-1","point":null,"value":{"kind":"failure","error":{"kind":"trajectory","reason":"BeforeFirstOnset"}},"control":{"requested":1,"successful":0,"contributing":0,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"failure","error":{"kind":"insufficient","level":"query[3] controls","requested":1,"successful":0,"minimum":1}}},"bin":null},{"index":4,"timeMicros":"20001","point":null,"value":{"kind":"failure","error":{"kind":"trajectory","reason":"AfterLastOnset"}},"control":{"requested":1,"successful":0,"contributing":0,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"failure","error":{"kind":"insufficient","level":"query[4] controls","requested":1,"successful":0,"minimum":1}}},"bin":null}],"bins":[{"index":0,"fromMicros":"0","untilMicros":"10000","includesFinalEndpoint":false,"queries":[1],"observed":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.7071067811865475}},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.4752931878933584}},"difference":{"kind":"success","value":-0.23181359329318907}},{"index":1,"fromMicros":"10000","untilMicros":"20000","includesFinalEndpoint":true,"queries":[0,2],"observed":{"requested":2,"successful":2,"contributing":2,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.7071067811865475}},"control":{"requested":2,"successful":2,"contributing":2,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.8555277382080452}},"difference":{"kind":"success","value":0.1484209570214977}}]},{"index":2,"key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"source"}},"template":{"kind":"success","key":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"C","phase":"template"}}},"eligibleControls":2,"selectedControls":1,"controls":[{"occurrence":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"B","phase":"source"}},"template":{"schema":{"name":"example.point-key","version":1},"value":{"participant":"p,\"λ\"\n","stimulus":"B","phase":"template"}},"values":[{"kind":"success","value":-0.7071067811865475},{"kind":"success","value":-0.7071067811865475},{"kind":"success","value":-0.7071067811865475},{"kind":"failure","error":{"kind":"trajectory","reason":"BeforeFirstOnset"}},{"kind":"failure","error":{"kind":"trajectory","reason":"AfterLastOnset"}}]}],"points":[{"index":0,"timeMicros":"20000","point":[1.5,0.5],"value":{"kind":"success","value":-0.8555277382080452},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.7071067811865475}},"bin":1},{"index":1,"timeMicros":"0","point":[0.5,0.5],"value":{"kind":"success","value":-0.4752931878933584},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.7071067811865475}},"bin":0},{"index":2,"timeMicros":"10000","point":[1.5,0.5],"value":{"kind":"success","value":-0.8555277382080452},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.7071067811865475}},"bin":1},{"index":3,"timeMicros":"-1","point":null,"value":{"kind":"failure","error":{"kind":"trajectory","reason":"BeforeFirstOnset"}},"control":{"requested":1,"successful":0,"contributing":0,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"failure","error":{"kind":"insufficient","level":"query[3] controls","requested":1,"successful":0,"minimum":1}}},"bin":null},{"index":4,"timeMicros":"20001","point":null,"value":{"kind":"failure","error":{"kind":"trajectory","reason":"AfterLastOnset"}},"control":{"requested":1,"successful":0,"contributing":0,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"failure","error":{"kind":"insufficient","level":"query[4] controls","requested":1,"successful":0,"minimum":1}}},"bin":null}],"bins":[{"index":0,"fromMicros":"0","untilMicros":"10000","includesFinalEndpoint":false,"queries":[1],"observed":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.4752931878933584}},"control":{"requested":1,"successful":1,"contributing":1,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.7071067811865475}},"difference":{"kind":"success","value":0.23181359329318907}},{"index":1,"fromMicros":"10000","untilMicros":"20000","includesFinalEndpoint":true,"queries":[0,2],"observed":{"requested":2,"successful":2,"contributing":2,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.8555277382080452}},"control":{"requested":2,"successful":2,"contributing":2,"policy":{"kind":"successfulOnly","minimum":1},"result":{"kind":"success","value":-0.7071067811865475}},"difference":{"kind":"success","value":-0.1484209570214977}}]}],"controlPairing":{"pairSpace":{"kind":"betweenDirected","relation":"(participant == participant and stimulus != stimulus)","selection":{"kind":"bottomK","cap":1,"seed":"-9223372036854775801","sampleId":"persisted-points"}},"eligiblePairCount":"6","selectedPairCount":3,"unmatchedLeft":[],"unmatchedRight":[],"ambiguous":[]},"unbinned":[3,4],"inputHash":"6b0d7f267940501e","planHash":"4069abc18c10519e","provenance":{"inputs":"6b0d7f267940501e","steps":[{"operation":"eyes4s.static-density-onset-sampling-arithmetic-mean/1","params":[{"name":"plan","kind":"text","value":"4069abc18c10519e"},{"name":"sourceRows","kind":"num","value":3.0},{"name":"queriesPerSource","kind":"num","value":5.0}]}]}}}}"""
  // format: on

  test("built-in v1 fixtures and independently calculated canonical stamp agree") {
    val a = archive
    assertEquals(
      family.plans.encode(a.plan),
      io.circe.parser
        .parse(pinnedPlan)
        .left
        .map(e => CodecError.InvalidJson(pinnedPlan, e.message))
    )
    assertEquals(
      family.results.encode(a),
      io.circe.parser
        .parse(pinnedResult)
        .left
        .map(e => CodecError.InvalidJson(pinnedResult, e.message))
    )
  }
