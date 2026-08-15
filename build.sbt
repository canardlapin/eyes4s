import org.typelevel.sbt.gha.JavaSpec
import org.typelevel.sbt.gha.{GenerativePlugin, PermissionValue, Permissions}

import scala.io.Source

// ---------------------------------------------------------------------------
// Versions
// ---------------------------------------------------------------------------

// Scala 3 LTS. A library published from the LTS line is consumable from every
// later 3.x; the reverse is not true. See bead fnd-build, PRD B-1.
val Scala3 = "3.3.8"

// eyes4s-kernel's ONLY external dependency is cats-core. There is deliberately
// no `algebra` and no Spire: `algebra` has no vector-space abstraction to
// inherit, and Spire is not published for Scala Native 0.5 and defines a
// length-zero `zero` for arrays, which is incompatible with a per-grid carrier.
// The module structure on Signed[U] is defined here instead -- see PRD O-12 and
// the "Core defines its own Module" resolution in eyes4s.md.
val catsV            = "2.13.0"
val circeV           = "0.14.16" // eyes4s-codec only
val munitV           = "1.3.4"
val munitScalacheckV = "1.3.0"
val disciplineMunitV = "2.0.0"
val scalacheckV      = "1.19.0"
val catsLawsV        = "2.13.0"
val catsEffectV      = "3.7.0"   // eyes4s-fs2 / eyes4s-io only
val fs2V             = "3.13.0"  // eyes4s-fs2 / eyes4s-io only

// ---------------------------------------------------------------------------
// Build-wide settings
// ---------------------------------------------------------------------------

ThisBuild / tlBaseVersion    := "0.1"
ThisBuild / organization     := "io.github.canardlapin"
ThisBuild / organizationName := "canardlapin"
ThisBuild / startYear        := Some(2026)
ThisBuild / licenses         := Seq(License.Apache2)
ThisBuild / developers       := List(
  tlGitHubDev("canardlapin", "canardlapin")
)

ThisBuild / scalaVersion       := Scala3
ThisBuild / crossScalaVersions := Seq(Scala3)
ThisBuild / tlJdkRelease       := Some(11)

ThisBuild / githubWorkflowJavaVersions := Seq(
  JavaSpec.temurin("17"),
  JavaSpec.temurin("21")
)

// Code health, security submission, and publication have different trigger and
// permission boundaries. Generate them as separate files from the same
// sbt-github-actions model so a missing credential cannot turn a green code
// check red, and a branch push can never enter the release path.
ThisBuild / githubWorkflowPublishTargetBranches := Seq.empty
ThisBuild / githubWorkflowTargetTags            := Seq.empty
ThisBuild / githubWorkflowAddedJobs             := Seq.empty
ThisBuild / githubWorkflowPermissions           := Some(
  Permissions.Specify.defaultRestrictive.withPackages(PermissionValue.None)
)

lazy val trustWorkflowContents =
  taskKey[Map[String, String]]("Render every generated eyes4s workflow.")

def bundledCleanWorkflow: String = {
  val stream = Option(GenerativePlugin.getClass.getResourceAsStream("/clean.yml"))
    .getOrElse(sys.error("sbt-github-actions clean.yml resource is unavailable"))
  val source = Source.fromInputStream(stream, "UTF-8")
  try source.mkString
  finally source.close()
}

trustWorkflowContents := {
  val sbtCommand      = githubWorkflowSbtCommand.value
  val basePermissions =
    Permissions.Specify.defaultRestrictive.withPackages(PermissionValue.None)

  val checks = GenerativePlugin.compileWorkflow(
    "Checks",
    githubWorkflowTargetBranches.value.toList,
    Nil,
    githubWorkflowTargetPaths.value,
    githubWorkflowPREventTypes.value.toList,
    Some(basePermissions),
    githubWorkflowEnv.value,
    githubWorkflowConcurrency.value,
    githubWorkflowGeneratedCI.value.toList,
    sbtCommand
  )

  val dependencySubmission = WorkflowJob(
    "dependency-submission",
    "Submit Dependencies",
    githubWorkflowJobSetup.value.toList ::: List(
      WorkflowStep.DependencySubmission(
        workingDirectory = None,
        modulesIgnore = Some(List("rootjs_3", "rootjvm_3", "rootnative_3")),
        configsIgnore = Some(List("test", "scala-tool", "scala-doc-tool", "test-internal")),
        token = None
      )
    ),
    sbtStepPreamble = Nil,
    cond = Some("github.event_name == 'push' && github.ref == 'refs/heads/main'"),
    permissions = Some(basePermissions.withContents(PermissionValue.Write)),
    oses = githubWorkflowOSes.value.toList.take(1),
    scalas = Nil,
    javas = githubWorkflowJavaVersions.value.toList.take(1)
  )

  val security = GenerativePlugin.compileWorkflow(
    "Security",
    List("main"),
    Nil,
    Paths.None,
    githubWorkflowPREventTypes.value.toList,
    Some(basePermissions),
    githubWorkflowEnv.value,
    githubWorkflowConcurrency.value,
    List(dependencySubmission),
    sbtCommand
  )

  val releaseJob = WorkflowJob(
    "release",
    "Publish Version Tag",
    githubWorkflowJobSetup.value.toList :::
      githubWorkflowPublishPreamble.value.toList :::
      githubWorkflowPublish.value.toList :::
      githubWorkflowPublishPostamble.value.toList,
    sbtStepPreamble = Nil,
    cond = Some("github.event_name == 'push' && startsWith(github.ref, 'refs/tags/v')"),
    permissions = Some(basePermissions),
    oses = githubWorkflowOSes.value.toList.take(1),
    scalas = Nil,
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    timeoutMinutes = githubWorkflowPublishTimeoutMinutes.value
  )

  val release = GenerativePlugin.compileWorkflow(
    "Release",
    List("release-workflow-disabled"),
    List("v*"),
    Paths.None,
    githubWorkflowPREventTypes.value.toList,
    Some(basePermissions),
    githubWorkflowEnv.value,
    githubWorkflowConcurrency.value,
    List(releaseJob),
    sbtCommand
  )

  Map(
    "checks.yml"   -> checks,
    "security.yml" -> security,
    "release.yml"  -> release,
    "clean.yml"    -> bundledCleanWorkflow
  )
}

githubWorkflowGenerate := {
  val outputDirectory = baseDirectory.value / ".github" / "workflows"
  val rendered        = trustWorkflowContents.value
  IO.createDirectory(outputDirectory)
  rendered.foreach { case (name, contents) => IO.write(outputDirectory / name, contents) }

  // ci.yml was the old mixed-permission workflow. Removal is part of the
  // generated migration, not a hand edit to generated YAML.
  IO.delete(outputDirectory / "ci.yml")
}

githubWorkflowCheck := {
  val outputDirectory = baseDirectory.value / ".github" / "workflows"
  val rendered        = trustWorkflowContents.value
  rendered.foreach { case (name, expected) =>
    val file = outputDirectory / name
    if (!file.exists()) sys.error(s"$name is missing; run githubWorkflowGenerate")
    val actual = IO.read(file)
    if (actual != expected)
      sys.error(s"$name is stale; run githubWorkflowGenerate")
  }
  val obsolete = outputDirectory / "ci.yml"
  if (obsolete.exists())
    sys.error("ci.yml still mixes code, security, and release jobs; run githubWorkflowGenerate")

  def requireText(workflow: String, required: String): Unit = {
    if (!rendered(workflow).contains(required))
      sys.error(s"$workflow is missing required generated content: $required")
  }

  def forbidText(workflow: String, forbidden: String): Unit = {
    if (rendered(workflow).contains(forbidden))
      sys.error(s"$workflow contains forbidden generated content: $forbidden")
  }

  List(
    "project: [rootJS, rootJVM]",
    "name: Compile all modules",
    "name: Test",
    "name: Run published law suites",
    "name: Check headers and formatting",
    "name: Check binary compatibility",
    "name: Generate API documentation",
    "name: Check module and kernel boundaries"
  ).foreach(requireText("checks.yml", _))
  forbidText("checks.yml", "tlCiRelease")
  forbidText("checks.yml", "sbt-dependency-submission")

  requireText("security.yml", "contents: write")
  requireText("security.yml", "scalacenter/sbt-dependency-submission@v2")
  forbidText("security.yml", "tlCiRelease")

  requireText("release.yml", "tags: [v*]")
  requireText("release.yml", "startsWith(github.ref, 'refs/tags/v')")
  requireText("release.yml", "sbt tlCiRelease")
  forbidText("release.yml", "sbt-dependency-submission")
}

// Both boundary invariants run in CI, not just on a developer's machine.
// A rule that is only checked locally is a rule that is checked when it is
// convenient. See bead fnd-boundaries, PRD B-9.
ThisBuild / githubWorkflowBuild += WorkflowStep.Sbt(
  List("compile"),
  name = Some("Compile all modules")
)

ThisBuild / githubWorkflowBuild += WorkflowStep.Sbt(
  List("lawsJVM/test", "lawsJS/test"),
  name = Some("Run published law suites"),
  cond = Some("matrix.project == 'rootJVM' && matrix.java == 'temurin@17'"),
  preamble = false
)

ThisBuild / githubWorkflowBuild += WorkflowStep.Sbt(
  List("checkBoundaries"),
  name = Some("Check module and kernel boundaries")
)

// Scala Native is deferred post-1.0 (bead q-app-target: the application target
// is a local JVM process serving a browser UI, so Native buys nothing). Every
// dependency is nonetheless kept Native-eligible so that adding the axis later
// stays a build change rather than a redesign. See PRD B-4.

// ---------------------------------------------------------------------------
// Module boundary enforcement                              (bead fnd-boundaries)
//
// Two invariants, both checked mechanically against the build graph, because
// documentation is not enforcement. See PRD PKG-4.
//
//   PKG-3  No pure module may acquire an effect system, directly or
//          transitively. Cats Effect and FS2 belong to eyes4s-fs2 and
//          eyes4s-io alone.
//
//   PKG-1  eyes4s-kernel may not depend on eyes4s-core. This is what keeps the
//          trajectory-and-measure layer free of ocular vocabulary, and it is
//          the entire cost of leaving the door open to extracting a general
//          `trace4s` later. See eyes4s.md, "Beyond gaze: the trajectory kernel".
// ---------------------------------------------------------------------------

lazy val checkModuleBoundaries =
  taskKey[Unit]("Fail if a pure module depends on an effect library.")

lazy val checkKernelPurity =
  taskKey[Unit]("Fail if eyes4s-kernel source contains ocular vocabulary.")

// The DEPENDENCY direction (kernel must not depend on core) needs no task:
// core already depends on kernel, so a kernel -> core edge is a cycle and sbt
// rejects the build at load. What is NOT structurally prevented -- and is the
// failure mode that actually matters -- is someone declaring `Fixation` or
// `Saccade` inside the kernel. That is a CONTENT violation, and it is what
// PRD A-3 describes. This word list is that documented list, made executable.
val ocularVocabulary = Set(
  "gaze",
  "fixation",
  "saccade",
  "blink",
  "pursuit",
  "scanpath",
  "pupil",
  "ocular",
  "fovea",
  "foveal",
  "microsaccade",
  "vergence",
  "eyetrack",
  "eyelink",
  "tobii",
  "viewing",
  "cyclopean",
  "nystagmus"
)

def forbiddenInPureModules(org: String, name: String): Boolean =
  (org == "org.typelevel" && name.startsWith("cats-effect")) ||
    org == "co.fs2"

lazy val pureModuleSettings = Seq(
  checkModuleBoundaries := {
    val moduleName = name.value
    val offenders  = update.value.allModules
      .filter(m => forbiddenInPureModules(m.organization, m.name))
      .map(m => s"${m.organization}:${m.name}:${m.revision}")
      .distinct
      .sorted
    if (offenders.nonEmpty)
      sys.error(
        s"""|Module boundary violation in $moduleName.
            |
            |Pure modules must not depend on an effect system, but the resolved
            |dependency graph contains:
            |${offenders.map("  - " + _).mkString("\n")}
            |
            |Move the offending code to eyes4s-fs2 or eyes4s-io.""".stripMargin
      )
    else
      streams.value.log.info(s"$moduleName: module boundaries OK (no effect deps)")
  }
)

// Comments are stripped before scanning, so scaladoc may still explain the
// boundary in the words it is drawing. Only declarations are checked.
ThisBuild / checkKernelPurity := {
  val log     = streams.value.log
  val srcRoot = (ThisBuild / baseDirectory).value / "kernel" / "src"

  // Blank out comments while PRESERVING newlines, so reported line numbers
  // still refer to the real file rather than to the stripped body.
  def blankOut(m: scala.util.matching.Regex.Match): String =
    m.matched.map(c => if (c == '\n') '\n' else ' ')

  def stripComments(s: String): String = {
    val noBlock = "(?s)/\\*.*?\\*/".r.replaceAllIn(s, blankOut _)
    "//[^\n]*".r.replaceAllIn(noBlock, blankOut _)
  }

  val pattern = ocularVocabulary.mkString("(?i)\\b(", "|", ")\\w*\\b").r

  val sources =
    if (srcRoot.exists) (srcRoot ** "*.scala").get else Nil

  val offenders = sources
    .flatMap { f =>
      val body = stripComments(IO.read(f))
      pattern.findAllMatchIn(body).map { m =>
        val line = body.take(m.start).count(_ == '\n') + 1
        s"  - ${f.getName}:$line  '${m.matched}'"
      }
    }
    .distinct
    .sorted

  if (offenders.nonEmpty)
    sys.error(
      s"""|Kernel purity violation.
          |
          |eyes4s-kernel is the trajectory-and-measure layer: it describes timed
          |paths through a typed geometry and the measures they induce. It must
          |contain no ocular vocabulary, so that a general `trace4s` -- mouse
          |tracking, animal tracking, VR navigation -- stays extractable.
          |
          |Found in kernel sources:
          |${offenders.mkString("\n")}
          |
          |Either move this to eyes4s-core, or generalise the concept until it
          |belongs in the kernel. See eyes4s.md, "Beyond gaze: the trajectory
          |kernel", and PRD A-3.""".stripMargin
    )
  else
    log.info(s"kernel purity OK (${sources.size} source(s) scanned, no ocular vocabulary)")
}

lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit"            % munitV           % Test,
    "org.scalameta" %%% "munit-scalacheck" % munitScalacheckV % Test
  )
)

// ---------------------------------------------------------------------------
// Modules
// ---------------------------------------------------------------------------

lazy val root = tlCrossRootProject
  .aggregate(
    kernel,
    core,
    detect,
    surface,
    aoi,
    compare,
    design,
    plan,
    codec,
    laws,
    fs2Module,
    io
  )

/** Geometry, time, trajectories, measures, grids, surfaces, regions, machines.
  *
  * No ocular vocabulary. Enforced by `checkKernelPurity`.
  */
lazy val kernel = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("kernel"))
  .settings(commonSettings, pureModuleSettings)
  .settings(
    name                                    := "eyes4s-kernel",
    libraryDependencies += "org.typelevel" %%% "cats-core" % catsV
  )

/** The eye-specific layer: Gaze, Sample, Recording, BinocularRecording, Event,
  * Scanpath, Viewing.
  */
lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .dependsOn(kernel)
  .settings(commonSettings, pureModuleSettings)
  .settings(name := "eyes4s-core")

/** Detector instances, filters, and the Machine runner. */
lazy val detect = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("detect"))
  .dependsOn(core)
  .settings(commonSettings, pureModuleSettings)
  .settings(name := "eyes4s-detect")

/** Smoothers, bandwidth selection, pyramids, entropy. */
lazy val surface = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("surface"))
  .dependsOn(core)
  .settings(commonSettings, pureModuleSettings)
  .settings(name := "eyes4s-surface")

/** AOI sets, dwell/entry/run statistics, transition matrices. */
lazy val aoi = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("aoi"))
  .dependsOn(core)
  .settings(commonSettings, pureModuleSettings)
  .settings(name := "eyes4s-aoi")

/** Compare hierarchy, alignment kernel, MultiMatch, distribution measures, OT. */
lazy val compare = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("compare"))
  .dependsOn(core, surface, aoi)
  .settings(commonSettings, pureModuleSettings)
  .settings(name := "eyes4s-compare")

/** Trials, pairings, baselines, contrasts, deterministic RNG. */
lazy val design = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("design"))
  .dependsOn(core, compare)
  .settings(commonSettings, pureModuleSettings)
  .settings(name := "eyes4s-design")

/** Analyses as descriptions: plan ADTs, interpreters, and the typed registry. */
lazy val plan = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("plan"))
  .dependsOn(design)
  .settings(commonSettings, pureModuleSettings)
  .settings(name := "eyes4s-plan")

/** JSON codecs with a versioned schema, so a project file round-trips. */
lazy val codec = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("codec"))
  .dependsOn(plan)
  .settings(commonSettings, pureModuleSettings)
  .settings(
    name := "eyes4s-codec",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core"   % circeV,
      "io.circe" %%% "circe-parser" % circeV
    )
  )

/** Discipline rule sets and ScalaCheck generators.
  *
  * munit, scalacheck and discipline are MAIN-scope dependencies here, not Test:
  * the suites are library code that downstream authors run against their own
  * instances. See PRD PKG-6.
  */
lazy val laws = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("laws"))
  .dependsOn(kernel, core, detect, surface, aoi, compare, design)
  .settings(pureModuleSettings)
  .settings(
    name := "eyes4s-laws",
    libraryDependencies ++= Seq(
      "org.scalameta"  %%% "munit"            % munitV,
      "org.scalameta"  %%% "munit-scalacheck" % munitScalacheckV,
      "org.typelevel"  %%% "discipline-munit" % disciplineMunitV,
      "org.typelevel"  %%% "cats-laws"        % catsLawsV,
      "org.scalacheck" %%% "scalacheck"       % scalacheckV
    )
  )

/** Streaming execution: Machine.toPipe, progress events, cancellation. */
lazy val fs2Module = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("fs2"))
  .dependsOn(core, detect, plan)
  .settings(commonSettings)
  .settings(
    name := "eyes4s-fs2",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "co.fs2"        %%% "fs2-core"    % fs2V
    )
  )

/** Ingest and export: EyeLink ASC, CSV, Mirror-derived metadata decoders. */
lazy val io = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("io"))
  .dependsOn(fs2Module, codec)
  .settings(commonSettings)
  .settings(
    name                             := "eyes4s-io",
    libraryDependencies += "co.fs2" %%% "fs2-io" % fs2V
  )

// ---------------------------------------------------------------------------
// Aliases
// ---------------------------------------------------------------------------

lazy val allModules = Seq(
  "kernel",
  "core",
  "detect",
  "surface",
  "aoi",
  "compare",
  "design",
  "plan",
  "codec",
  "laws",
  "fs2Module",
  "io"
)
lazy val allPlatforms = Seq("JVM", "JS")

addCommandAlias(
  "compileAll",
  allModules.flatMap(m => allPlatforms.map(p => s"$m$p/compile")).mkString(";", ";", "")
)

addCommandAlias(
  "testAll",
  allModules.flatMap(m => allPlatforms.map(p => s"$m$p/test")).mkString(";", ";", "")
)

// checkModuleBoundaries is per-module (it inspects each module's own resolved
// graph); checkKernelPurity is build-level. `checkBoundaries` runs both.
addCommandAlias(
  "checkBoundaries",
  (Seq("checkKernelPurity") ++
    allModules
      .filterNot(m => m == "fs2Module" || m == "io")
      .flatMap(m => allPlatforms.map(p => s"$m$p/checkModuleBoundaries")))
    .mkString(";", ";", "")
)
