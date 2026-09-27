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

package eyes4s.studio.core.document

import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.studio.core.backend.*
import org.scalacheck.Gen

/** Generators of valid document values, each built through its smart
  * constructor, and of whole documents that are consistent by construction.
  */
object DocumentGen:

  def right[A](e: Either[DocumentError, A]): A =
    e.fold(l => throw new AssertionError(l.message), identity)

  private def valid[A](g: Gen[Either[DocumentError, A]]): Gen[A] =
    g.suchThat(_.isRight).map(right)

  val hex64: Gen[String] = Gen.listOfN(64, Gen.hexChar.map(_.toLower)).map(_.mkString)
  val hex16: Gen[String] = Gen.listOfN(16, Gen.hexChar.map(_.toLower)).map(_.mkString)

  val word: Gen[String] =
    Gen.choose(1, 8).flatMap(n => Gen.listOfN(n, Gen.alphaNumChar).map(_.mkString))

  /** Text with non-ASCII, quotes and escapes, to exercise the codecs. */
  val text: Gen[String] = Gen.oneOf(
    word,
    Gen.const("Encoding → retrieval · σ 2°"),
    Gen.const("quote \" backslash \\ tab \t"),
    Gen.const("")
  )

  val byteDigest: Gen[ByteDigest]           = hex64.map(h => ByteDigest.parse(h).toOption.get)
  def canonical[A]: Gen[CanonicalDigest[A]] =
    hex64.map(h => CanonicalDigest.parse[A](h).toOption.get)
  def binding[A]: Gen[CoreBinding[A]] =
    Gen.oneOf(Gen.const(CoreBinding.unbound[A]), canonical[A].map(CoreBinding.Bound(_)))

  val phase: Gen[Phase]       = Gen.oneOf(Phase.Encoding, Phase.Retrieval, Phase("Recognition"))
  val trialKey: Gen[TrialKey] =
    for
      p <- Gen.choose(1, 24).map(i => f"P$i%02d")
      f <- phase
      t <- Gen.choose(1, 20).map(i => f"ret_$i%02d")
      o <- Gen.choose(1, 3)
    yield TrialKey(p, f, t, o)

  // --- Sources and datasets -------------------------------------------------

  val sourcePath: Gen[SourcePath] =
    Gen.nonEmptyListOf(word).map(ws => right(SourcePath.of(ws.take(3).mkString("/"))))
  val semantic: Gen[SemanticIdentity]       = hex16.map(h => right(SemanticIdentity.of(h)))
  def source(role: SourceRole): Gen[Source] =
    for
      path <- sourcePath
      d    <- byteDigest
      s    <- Gen.option(semantic)
    yield Source(role, path, d, s)
  val sources: Gen[Sources] = valid(
    for
      f <- source(SourceRole.Fixations)
      t <- Gen.option(source(SourceRole.Trials))
    yield Sources.of(Vector(f) ++ t)
  )

  val mapping: Gen[ColumnMapping] =
    for
      optional <- Gen.someOf(
        ColumnRole.values.toVector.filterNot(ColumnRole.required.contains)
      )
      roles = ColumnRole.required ++ optional
      shuffled <- Gen.pick(roles.size, roles)
    yield right(
      ColumnMapping.of(
        shuffled.toVector.map(r => ColumnBinding(r, right(ColumnName.of(s"col_${r.label}"))))
      )
    )

  /** Attribute columns beside `m`'s columns (none of them mapped). */
  def attributesFor(m: ColumnMapping): Gen[DeclaredAttributes] =
    for
      n     <- Gen.choose(0, 3)
      kinds <- Gen.listOfN(n, Gen.oneOf(AttributeKindChoice.values.toSeq))
    yield right(
      DeclaredAttributes.of(
        kinds.zipWithIndex.toVector
          .map((k, i) => AttributeBinding(right(ColumnName.of(s"attr_$i")), k))
          .filterNot(a => m.bindings.exists(_.column == a.column))
      )
    )

  val units: Gen[DeclaredUnits] =
    Gen.option(Gen.oneOf(TimeUnit.values.toSeq)).map(DeclaredUnits(_))

  val geometry: Gen[Geometry] = valid(
    for
      w   <- Gen.choose(800, 4000)
      h   <- Gen.choose(600, 3000)
      iw  <- Gen.choose(1, w)
      ih  <- Gen.choose(1, h)
      l   <- Gen.choose(0, w - iw)
      t   <- Gen.choose(0, h - ih)
      ppd <- Gen.choose(1.0, 80.0)
    yield
      for
        s <- ScreenSize.of(w, h)
        i <- ImagePlacement.of(l, t, iw, ih)
        p <- DeclaredPixelsPerDegree.of(ppd)
        g <- Geometry.of(s, i, p)
      yield g
  )

  val correction: Gen[CoordinateCorrection] = Gen.oneOf(
    Gen.const(CoordinateCorrection.FlipX),
    Gen.const(CoordinateCorrection.FlipY),
    valid(Gen.zip(Gen.choose(-50.0, 50.0), Gen.choose(-50.0, 50.0)).map(Offset.of))
      .map(CoordinateCorrection.Translate(_))
  )
  val target: Gen[CorrectionTarget] = Gen.oneOf(
    Gen.const(CorrectionTarget.AllTrials),
    word.map(w => CorrectionTarget.Participant(right(ParticipantId.of(w)))),
    trialKey.map(CorrectionTarget.Trial(_))
  )
  val admission: Gen[AdmissionChoice] =
    for
      o  <- Gen.oneOf(OffScreenChoice.values.toSeq)
      cs <- Gen.listOfN(2, Gen.zip(target, correction)).flatMap(Gen.someOf(_))
    yield AdmissionChoice(o, cs.toVector.map(CorrectionRule.apply))
  val decision: Gen[AdmissionDecision] = Gen.oneOf(
    Gen.const(AdmissionDecision.Pending),
    canonical[DatasetRevisionSpec].map(AdmissionDecision.Verifying(_)),
    Gen
      .zip(binding[AdmissionLedgerArtifact], binding[TrialInventoryArtifact])
      .map(AdmissionDecision.Admitted.apply)
  )

  def dataset(id: Int, earlier: Vector[Int]): Gen[DatasetRevisionSpec] =
    for
      parent <- if earlier.isEmpty then Gen.const(None) else Gen.option(Gen.oneOf(earlier))
      s      <- sources
      m      <- mapping
      u      <- units
      g      <- geometry
      a      <- admission
      d      <- decision
      at     <- attributesFor(m)
    yield DatasetRevisionSpec(
      DatasetRevision(id),
      parent.map(DatasetRevision(_)),
      s,
      m,
      u,
      g,
      a,
      d,
      at
    )

  // --- Analyses -------------------------------------------------------------

  val sigma: Gen[Sigma] =
    Gen.oneOf(0.5, 1.0, 2.0, 4.0, 8.0, 0.25, 1.5).map(d => right(Sigma.of(d)))
  val scales: Gen[ScaleSet] =
    Gen.nonEmptyContainerOf[Set, Sigma](sigma).flatMap(s => Gen.pick(s.size, s.toSeq)).map {
      s =>
        right(ScaleSet.of(s.toVector))
    }
  val grid: Gen[GridSize] =
    Gen.zip(Gen.choose(1, 128), Gen.choose(1, 96)).map((c, r) => right(GridSize.of(c, r)))
  val initial: Gen[InitialFixationChoice] = Gen.oneOf(
    Gen.const(InitialFixationChoice.KeepAll),
    Gen.const(InitialFixationChoice.DropFirst),
    Gen
      .oneOf(0.5, 1.5, 2.0)
      .map(r => InitialFixationChoice.DropLeadingNearCross(right(CrossRadius.of(r))))
  )
  val pick: Gen[OccurrencePick] = Gen.oneOf(
    Gen.const(OccurrencePick.First),
    Gen.const(OccurrencePick.Last),
    Gen.choose(1, 4).map(n => OccurrencePick.At(right(Occurrence.of(n))))
  )
  val matched: Gen[MatchedChoice] = Gen.oneOf(
    Gen.const(MatchedChoice.RequireOne),
    Gen.const(MatchedChoice.SameOccurrence),
    pick.map(MatchedChoice.Select(_)),
    Gen.const(MatchedChoice.MeanOfAll)
  )
  val definition: Gen[DefinitionRef] =
    Gen
      .zip(
        Gen.oneOf("eyes4s.cosine", "eyes4s.participant-stimulus-phase", "lab.method"),
        Gen.choose(1, 3)
      )
      .map((n, v) => right(DefinitionRef.of(n, v)))
  val method: Gen[MethodSpec] =
    for
      d  <- definition
      ps <- Gen
        .listOfN(2, Gen.zip(word, text).map(MethodParameter.apply))
        .flatMap(Gen.someOf(_))
    yield MethodSpec(d, ps.toVector)
  val failure: Gen[FailureChoice] = Gen.oneOf(
    Gen.const(FailureChoice.RequireAll),
    Gen.choose(1, 5).map(n => FailureChoice.SuccessfulOnly(right(MinimumSuccessful.of(n))))
  )
  val window: Gen[AnalysisWindow] =
    for
      x0 <- Gen.choose(0.0, 900.0)
      y0 <- Gen.choose(0.0, 500.0)
      w  <- Gen.choose(1.0, 1000.0)
      h  <- Gen.choose(1.0, 500.0)
    yield right(AnalysisWindow.of(x0, y0, x0 + w, y0 + h))
  val recipe: Gen[Recipe] =
    for
      in <- Gen.option(semantic)
      l  <- definition
      me <- method
      ph <- Gen.zip(phase, phase).map(PhasePair.apply)
      w  <- Gen.oneOf(WeightChoice.values.toSeq)
      f  <- failure
      g  <- grid
      wi <- Gen.option(window)
      ow <-
        if wi.isEmpty then Gen.const(None)
        else Gen.some(Gen.oneOf(OffWindowChoice.values.toSeq))
      s <- scales
      a <- Gen.option(
        Gen.oneOf(20.0, 35.0, 40.5).map(v => right(DeclaredPixelsPerDegree.of(v)))
      )
      m <- matched
      c <- Gen.oneOf(ControlChoice.values.toSeq)
      u <- Gen.oneOf(UnmatchedChoice.values.toSeq)
      i <- initial
    yield Recipe(in, l, me, ph, w, f, g, wi, ow, s, a, m, c, u, i)
  val studio: Gen[StudioFields] =
    for
      p <- Gen.oneOf(Preset.values.toSeq)
      n <- word.map(w => right(RevisionName.of(w)))
      d <- text
    yield StudioFields(p, n, d)

  def analysis(id: Int, datasets: Vector[Int]): Gen[AnalysisRevisionSpec] =
    for
      d <- Gen.oneOf(datasets)
      b <- binding[StudyPlanArtifact]
      r <- recipe
      s <- studio
    yield AnalysisRevisionSpec(AnalysisRevision(id), DatasetRevision(d), b, r, s)

  val change: Gen[RecipeChange] =
    Gen.zip(recipe, recipe).map(RecipeChange.between).suchThat(_.nonEmpty).flatMap(Gen.oneOf(_))

  /** A draft of `base`, rebased onto one of `admitted` other than its own
    * dataset some of the time.
    */
  def draft(
      base: AnalysisRevisionSpec,
      id: Int,
      admitted: Vector[DatasetRevision] = Vector.empty
  ): Gen[Option[Draft]] =
    val targets = admitted.filterNot(_ == base.dataset)
    for
      r <- recipe
      d <- if targets.isEmpty then Gen.const(None) else Gen.option(Gen.oneOf(targets))
    yield Draft.between(AnalysisRevision(id), base, r, d).toOption

  // --- Runs, reporting, figures ---------------------------------------------

  val lifecycle: Gen[RunLifecycle] = Gen.oneOf(
    Gen.const(RunLifecycle.Running),
    Gen.const(RunLifecycle.Completed),
    Gen.option(Gen.oneOf(StageKind.values.toSeq)).map(RunLifecycle.Cancelled(_)),
    Gen.const(RunLifecycle.Failed)
  )

  val covariate: Gen[Covariate]    = word.map(w => right(Covariate.of(w)))
  val filter: Gen[ReportingFilter] = Gen.oneOf(
    for
      a  <- covariate
      vs <- Gen.nonEmptyListOf(word)
    yield ReportingFilter.Keep(a, right(ValueSet.of(a, vs.toVector))),
    Gen.choose(0.0, 1.0).map(s => ReportingFilter.OutsideWindowAtMost(right(Share.of(s))))
  )
  def reporting(id: String): Gen[ReportingSpec] =
    for
      name <- word
      g    <- Gen.option(covariate)
      fs   <- Gen.listOfN(2, filter).flatMap(Gen.someOf(_))
      m    <- Gen.option(Gen.choose(1, 10).map(n => right(MinimumPerGroup.of(n))))
      w    <- Gen.oneOf(ReportingWeight.values.toSeq)
    yield right(ReportingSpec.of(right(ReportingId.of(id)), name, g, fs.toVector, m, w))

  def panelScale(scales: ScaleSet): Gen[PanelScale] = Gen.oneOf(
    Gen.const(PanelScale.Unscaled),
    Gen.oneOf(scales.values).map(PanelScale.At(_)),
    Gen.const(PanelScale.AllScales)
  )
  val selection: Gen[PanelSelection] = Gen.oneOf(
    trialKey.map(PanelSelection.Trial(_)),
    trialKey.map(PanelSelection.QueryWithReferences(_)),
    Gen.const(PanelSelection.AllQueries)
  )
  def panels(scales: ScaleSet): Gen[Vector[PanelSpec]] =
    Gen.choose(1, 5).flatMap { n =>
      Gen.sequence[Vector[PanelSpec], PanelSpec](
        ('A' until ('A' + n).toChar).map { l =>
          Gen
            .zip(text, panelScale(scales), selection)
            .map((t, s, sel) => PanelSpec(right(PanelLetter.of(l.toString)), t, s, sel))
        }
      )
    }

  // --- Presentation ---------------------------------------------------------

  def presentation(runs: Vector[RunId]): Gen[PresentationState] =
    for
      p  <- Gen.oneOf(Perspective.values.toSeq)
      t  <- Gen.oneOf(Theme.values.toSeq)
      s  <- Gen.oneOf(StageAppearance.values.toSeq)
      o  <- Gen.choose(0.0, 1.0).map(v => right(MapOpacity.of(v)))
      u  <- Gen.oneOf(true, false)
      r  <- if runs.isEmpty then Gen.const(None) else Gen.option(Gen.oneOf(runs))
      ls <- Gen
        .someOf(Perspective.values.toSeq)
        .flatMap(ps =>
          Gen.sequence[Vector[SavedLayout], SavedLayout](
            ps.map(p => text.map(b => SavedLayout(p, LayoutBlob(b))))
          )
        )
    yield right(PresentationState.of(p, t, s, o, u, r, ls))

  // --- Whole documents ------------------------------------------------------

  val document: Gen[StudioDocument] =
    for
      nd <- Gen.choose(1, 3)
      ds <- Gen.sequence[Vector[DatasetRevisionSpec], DatasetRevisionSpec](
        (1 to nd).map(i => dataset(i, (1 until i).toVector))
      )
      na <- Gen.choose(1, 3)
      as <- Gen.sequence[Vector[AnalysisRevisionSpec], AnalysisRevisionSpec](
        (1 to na).map(i => analysis(i, (1 to nd).toVector))
      )
      admitted = ds.filter(_.decision.isAdmitted).map(_.id)
      dr <- Gen
        .oneOf(as)
        .flatMap(base => Gen.option(draft(base, na + 1, admitted)).map(_.flatten))
      nr <- Gen.choose(0, 3)
      rs <- Gen.sequence[Vector[RunRef], RunRef](
        (1 to nr).map { i =>
          for
            a  <- Gen.oneOf(as)
            d  <- Gen.choose(1, nd)
            st <- lifecycle
            b  <- binding[ResultArchiveArtifact]
          yield RunRef(RunId(i), a.id, DatasetRevision(d), st, b)
        }
      )
      reps <- Gen
        .choose(1, 2)
        .flatMap(n =>
          Gen.sequence[Vector[ReportingSpec], ReportingSpec](
            (0 until n).map(i => reporting(s"rep-$i"))
          )
        )
      nf <- if rs.isEmpty then Gen.const(0) else Gen.choose(0, 2)
      fs <- Gen.sequence[Vector[FigureSpec], FigureSpec](
        (1 to nf).map { i =>
          for
            run <- Gen.oneOf(rs)
            rep <- Gen.oneOf(reps)
            ps  <- panels(as.find(_.id == run.analysis).get.recipe.scales)
          yield right(FigureSpec.of(right(FigureId.of(i)), run.id, rep.id, ps))
        }
      )
      view <- presentation(rs.map(_.id))
      js   <- jobs(rs)
    yield right(StudioDocument.of(ds, as, dr, rs, reps, fs, view, js))

  /** Job handles for some of the running runs. */
  def jobs(runs: Vector[RunRef]): Gen[Vector[JobHandle]] =
    Gen
      .someOf(runs.filter(_.state == RunLifecycle.Running))
      .flatMap(rs =>
        Gen.sequence[Vector[JobHandle], JobHandle](
          rs.map(r => Gen.choose(1, 99).map(j => JobHandle(r.id, JobId(j))))
        )
      )
