#!/usr/bin/env python3
"""Emit tests containing the exact current README and ASC guide fences."""
from pathlib import Path
import re
import sys
ROOT = Path(__file__).resolve().parents[1]
def blocks(name):
    return re.findall(r'^```scala\s*\n(.*?)^```', (ROOT / name).read_text(), re.M | re.S)
header = (ROOT / 'io/src/test/scala/eyes4s/io/TemplateFitSuite.scala').read_text().split('package ')[0]
readme = blocks('README.md')
asc = blocks('docs/formats/eyelink-asc.md')
assert len(readme) == 2 and len(asc) == 3, 'Review changed example inventory'
code = header + '''package docconsumer

import eyes4s.kernel.*
import eyes4s.core.*
import eyes4s.design.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import _root_.fs2.io.file.Files

class DocumentationExamplesSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

'''
def test(name, body):
    return '  test("' + name + '") {\n' + '\n'.join('    ' + line for line in body.splitlines()) + '\n  }\n\n'
code += test('README viewing geometry executes verbatim', 'val result = {\n' + readme[0] + '\n}\nassert(result.isRight)\nassert(Frame.screen("invalid", -1, 2).isLeft)')
code += test('README finite control design executes verbatim', readme[1] + '\nassert(controls.isRight)\nassert(Pairing.within[TrialKey].excludingSelf.bottomK(-1, Seed(1L), SampleId("bad")).isLeft)')
body = asc[0] + '''
assert(setup.isRight)
val (_, streamSettings, sessionConfig) = get(setup)
val ascBytes = IArray.from("START 1 LEFT SAMPLES\\nSAMPLES GAZE LEFT RATE 1000\\nPUPIL AREA\\n1 10 20 30\\nEND 2\\n".getBytes(java.nio.charset.StandardCharsets.UTF_8))
''' + asc[1] + asc[2] + '''
assert(materialized.report.isReconciled)
assert(materialized.trusted.isLeft)
val fromFile = Files[IO].tempFile.use { path =>
  _root_.fs2.Stream.emits(ascBytes.toVector).through(Files[IO].writeAll(path)).compile.drain *>
    get(importRecording(path))
}.unsafeRunSync()
assert(fromFile.report.isReconciled)
assert(fromFile.trusted.isLeft) // placeholder digest cannot certify the bytes
assert(fromFile.errors.nonEmpty)
'''
code += test('ASC guide executes construction byte admission and resource-safe file import', body)
code += test('documented native template CLI fits and reopens its exact recipe', """
Files[IO].tempDirectory.use { path => IO {
  eyes4s.examples.TemplateFitCli.main(Array("fit", path.toString))
  eyes4s.examples.TemplateFitCli.main(Array("evaluate", path.toString))
  val text = java.nio.file.Files.readString(path.toNioPath.resolve("recipe.json"))
  val result = get(eyes4s.examples.TemplateFitGuide.evaluateNative(text))
  assertEqualsDouble(get(result.rows.head.result)._1, 7.0, 1e-12)
}}.unsafeRunSync()
""")
code += test('documented baseline artifact command writes every checked table', """
import eyes4s.io.csv
Files[IO].tempDirectory.use { path =>
  eyes4s.examples.BaselineExportMain.run(List(path.toString)).map { exit =>
    assertEquals(exit, cats.effect.ExitCode.Success)
    val index = get(_root_.io.circe.parser.parse(java.nio.file.Files.readString(path.toNioPath.resolve("index.json"))))
    assertEquals(index.asArray.get.size, get(eyes4s.examples.BaselineExportGuide.tables).size)
    get(eyes4s.examples.BaselineExportGuide.tables).foreach { (name, table) =>
      assertEquals(java.nio.file.Files.readString(path.toNioPath.resolve(name + ".csv")), table.csv.encode)
      assert(java.nio.file.Files.size(path.toNioPath.resolve(name + ".arrow")) > 0)
    }
  }
}.unsafeRunSync()
""")
out = Path(sys.argv[1]); out.parent.mkdir(parents=True, exist_ok=True); out.write_text(code)
print(out)
