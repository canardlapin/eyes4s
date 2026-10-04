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

package example

import eyes4s.codec.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.Tolerance
import eyes4s.plan.*
import io.circe.Json

/** The templates page from a fresh consumer. Trial-level template fitting
  * (`Template.fit` and `Template.crossValidate`, for fixed features and for the
  * training-mean map) and cellwise surface decomposition (`Template.decompose`,
  * `decomposeNonNegative`, `decomposeMixture` and `PartialAssociation.of`) are
  * each checked against the same estimand composed explicitly: the least-squares
  * kernels on rows the consumer builds, or arithmetic the consumer does itself.
  * Runs on the JVM and Scala.js.
  */
class TemplateJourneySuite extends munit.FunSuite:
  private val Oracle = Tolerance(absolute = 1e-12, relative = 1e-12)

  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def near(actual: Double, expected: Double, what: String): Unit =
    assert(Oracle.approxEquals(actual, expected), s"$what: $actual != $expected")
  private val runtime = if System.getProperty("java.vm.name") == "Scala.js" then "js" else "jvm"

  // ---------------------------------------------------------------- trial-level fits

  test("a fixed-feature recipe cross-validates as fit then evaluate, and reopens unchanged") {
    val split = get(
      for
        basis <- TemplateBasis.of("journey-fixed/1", Vector("a", "b"), "score")
        a     <- TemplateObservation.of("a", "train", Vector(1.0, 0.0), 1.0)
        b     <- TemplateObservation.of("b", "train", Vector(0.0, 1.0), 2.0)
        c     <- TemplateObservation.of("c", "train", Vector(1.0, 1.0), 3.5)
        held  <- TemplateObservation.of("held", "test", Vector(3.0, 2.0), 700.0)
        split <- TemplateSplit.of(
          TemplateDesign.fixed(basis),
          Vector(a, b, c, held),
          Set("test")
        )
      yield split
    )
    val validated = get(Template.crossValidate(split))
    val fitted    = get(Template.fit(split.training))
    val evaluated = get(fitted.evaluate(split.heldOut))
    assertEquals(validated.fitted.coefficients, fitted.coefficients)
    assertEquals(validated.evaluation.rows, evaluated.rows)
    // Explicitly: least squares through the origin on the training features.
    val explicit = get(
      LeastSquares.fit(split.training.rows.map(_.input), split.training.rows.map(_.response))
    )
    fitted.coefficients.zip(explicit.coefficients).foreach((x, y) => near(x, y, "coefficient"))
    // Normal equations by hand: [[2,1],[1,2]] beta = [4.5, 5.5].
    near(fitted.coefficients(0), 7.0 / 6.0, "beta a")
    near(fitted.coefficients(1), 13.0 / 6.0, "beta b")
    val (prediction, residual) = get(evaluated.rows.head.result)
    near(prediction, 3.0 * 7.0 / 6.0 + 2.0 * 13.0 / 6.0, "prediction")
    near(residual, 700.0 - prediction, "residual")

    val codec = TemplateRecipeCodec.of[String, Vector[Double]](
      get(DefinitionId.of("example.journey-template", 1)),
      VersionedCodec.string(get(DefinitionId.of("example.journey-template-key", 1)))
    )
    val reopened = get(codec.decode(get(codec.encode(split))))
    assertEquals(get(Template.crossValidate(reopened)).evaluation.rows, evaluated.rows)
  }

  private val frame = get(Frame.screen("template-journey", 3, 2))
  private val grid  = get(Grid.over(frame, 3, 2))

  private def mass(name: String, weights: Double*): Mass[Px] =
    val values = IArray.from(weights)
    get(
      Surface
        .intensity(grid, values, Provenance.raw(ContentHash.ofString(name)))
        .flatMap(_.normalised)
    )

  private def cosine(a: Mass[Px], b: Mass[Px]): Double =
    val dot = a.values.indices.map(i => a.values(i) * b.values(i)).sum
    dot / math.sqrt(a.values.map(v => v * v).sum * b.values.map(v => v * v).sum)

  test("a training-mean map template equals the consumer's mean, cosines and slope") {
    val maps = Vector(
      ("p1", "a", mass("p1a", 4, 1, 0, 2, 2, 1), 2.0),
      ("p2", "a", mass("p2a", 3, 2, 1, 1, 2, 1), 1.5),
      ("p3", "b", mass("p3b", 0, 1, 3, 1, 2, 3), 0.5),
      ("p4", "b", mass("p4b", 1, 1, 2, 0, 3, 4), 0.25),
      ("p5", "a", mass("p5a", 2, 2, 2, 2, 1, 1), 4.0)
    )
    val rows = maps.map((p, item, m, response) =>
      get(
        TemplateObservation.of(
          p,
          if p == "p5" then "held" else "train",
          m,
          response,
          Some(item)
        )
      )
    )
    val design = get(TemplateDesign.meanMap[Px]("participant", "response units"))
    val split  = get(TemplateSplit.of(design, rows, Set("held")))
    // p1 and p2 share item `a` with the held-out p5, so they are excluded from training.
    assertEquals(split.training.rows.map(_.key), Vector("p3", "p4"))
    assertEquals(split.excluded.map(_.row.key), Vector("p1", "p2"))
    val validated = get(Template.crossValidate(split))

    val training = split.training.rows
    val mean     =
      IArray.tabulate(grid.size)(i => training.map(_.input.values(i)).sum / training.size)
    val learned = validated.fitted.template.getOrElse(fail("no learned template"))
    learned.values.indices.foreach(i => near(learned.values(i), mean(i), s"template cell $i"))
    val features = training.map(r => cosine(r.input, learned))
    val slope    =
      features.zip(training.map(_.response)).map(_ * _).sum / features.map(x => x * x).sum
    near(validated.fitted.coefficients.head, slope, "slope")
    val (prediction, _) = get(validated.evaluation.rows.head.result)
    near(prediction, slope * cosine(maps(4)._3, learned), "held-out prediction")
  }

  // ---------------------------------------------------------------- surface decomposition

  private val a     = mass("a", 4, 0, 0, 2, 2, 0)
  private val b     = mass("b", 0, 3, 1, 0, 1, 3)
  private val c     = mass("c", 1, 1, 4, 1, 0, 1)
  private val ids   = Vector("a", "b", "c").map(s => get(PredictorId.of(s)))
  private val set   = get(PredictorSet.of(ids.zip(Vector(a, b, c))))
  private val y     = mass("y", 3, 2, 1, 1, 4, 2)
  private val rows  = Vector.tabulate(grid.size)(i => Vector(a, b, c).map(_.values(i)))
  private val yvals = y.values.toVector

  test("OLS, NNLS and mixture fits equal the least-squares kernels on the same rows") {
    val ols         = get(Template.decompose(y, set, Intercept.Include))
    val explicitOls = get(LeastSquares.fit(rows.map(1.0 +: _), yvals))
    near(ols.intercept.get, explicitOls.coefficients.head, "intercept")
    ols.coefficients
      .map(_._2)
      .zip(explicitOls.coefficients.tail)
      .foreach((x, e) => near(x, e, "OLS"))
    ols.residual.values.toVector
      .zip(explicitOls.residuals)
      .foreach((x, e) => near(x, e, "OLS residual"))

    val nnls         = get(Template.decomposeNonNegative(y, set))
    val explicitNnls = get(ConstrainedLeastSquares.nonNegative(rows, yvals))
    nnls.coefficients
      .map(_._2)
      .zip(explicitNnls.coefficients)
      .foreach((x, e) => near(x, e, "NNLS"))
    assert(nnls.coefficients.forall(_._2 >= 0.0))
    nnls.fitted.values.toVector
      .zip(explicitNnls.fitted)
      .foreach((x, e) => near(x, e, "NNLS fit"))

    val mixture  = get(Template.decomposeMixture(y, set, Intercept.Include))
    val explicit = get(
      ConstrainedLeastSquares.simplex(rows.map(r => (1.0 / grid.size) +: r), yvals)
    )
    near(mixture.background.get, explicit.coefficients.head, "background")
    mixture.weights
      .map(_._2)
      .zip(explicit.coefficients.tail)
      .foreach((x, e) => near(x, e, "weight"))
    near(mixture.background.get + mixture.weights.map(_._2).sum, 1.0, "weights sum")
    near(mixture.fitted.values.sum, 1.0, "mixture fit is a mass")

    // An exact mixture is recovered as its weights.
    val mixed = get(
      Surface.mass(
        grid,
        IArray.tabulate(grid.size)(i => 0.25 * a.values(i) + 0.75 * b.values(i)),
        Provenance.raw(ContentHash.ofString("mixed"))
      )
    )
    val recovered = get(
      Template.decomposeMixture(
        mixed,
        get(PredictorSet.of(ids.take(2).zip(Vector(a, b)))),
        Intercept.Exclude
      )
    )
    near(recovered.weights(0)._2, 0.25, "recovered a")
    near(recovered.weights(1)._2, 0.75, "recovered b")
  }

  /** Average ranks, ties sharing the mean of their positions. */
  private def ranks(values: Vector[Double]): Vector[Double] =
    values.map(v => values.count(_ < v) + (values.count(_ == v) + 1) / 2.0)

  /** Pearson correlation of the residuals of `x` and `z` on an intercept and `w`. */
  private def partial(x: Vector[Double], z: Vector[Double], w: Vector[Double]): Double =
    val design = w.map(v => Vector(1.0, v))
    val rx     = get(LeastSquares.fit(design, x)).residuals
    val rz     = get(LeastSquares.fit(design, z)).residuals
    rx.zip(rz).map(_ * _).sum / math.sqrt(rx.map(v => v * v).sum * rz.map(v => v * v).sum)

  test("partial association is the explicit residual correlation, never a coefficient") {
    val covariates   = get(PredictorSet.of(Vector(ids(2) -> c)))
    val pearson      = get(PartialAssociation.of(y, a, covariates, AssociationMethod.Pearson))
    val spearman     = get(PartialAssociation.of(y, a, covariates, AssociationMethod.Spearman))
    val (yv, av, cv) = (yvals, a.values.toVector, c.values.toVector)
    near(pearson.estimate.get, partial(yv, av, cv), "partial Pearson")
    near(spearman.estimate.get, partial(ranks(yv), ranks(av), ranks(cv)), "partial Spearman")
    assertEquals(pearson.covariates, Vector(ids(2)))
    assertEquals(
      get(PartialAssociation.of(c, a, covariates, AssociationMethod.Pearson)).estimate,
      None
    )

    val decomposed = Vector(
      get(Template.decompose(y, set, Intercept.Include)).coefficients.map(_._2),
      get(Template.decomposeNonNegative(y, set)).coefficients.map(_._2),
      get(Template.decomposeMixture(y, set, Intercept.Include)).weights.map(_._2)
    )
    println(
      "EYES4S_TEMPLATE_JOURNEY=" + Json
        .obj(
          "runtime"        -> Json.fromString(runtime),
          "decompositions" -> Json.arr(
            decomposed.map(v => Json.arr(v.map(Json.fromDoubleOrNull)*))*
          ),
          "pearson"  -> Json.fromDoubleOrNull(pearson.estimate.get),
          "spearman" -> Json.fromDoubleOrNull(spearman.estimate.get)
        )
        .noSpaces
    )
  }
