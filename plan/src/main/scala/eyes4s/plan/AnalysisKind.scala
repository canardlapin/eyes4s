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

package eyes4s.plan

/** Every analysis the library offers, and the recipe family (if any) through
  * which an application can list, preflight, step, save and tabulate it.
  *
  * An analysis without a family runs only through its own eager entry point;
  * [[AnalysisKind.withoutFamily]] names those explicitly, and the family
  * conformance suite requires the kinds mapped to `None` to be exactly that
  * list, so a family that lands must shrink it.
  */
enum AnalysisKind derives CanEqual:
  /** Matched/control fixation-map similarity within participant. */
  case FixationStudy

  /** Synchronized, angular, detected and area-assigned recording. */
  case EventRecording

  /** The fixation study repeated over analysis windows and repetitions. */
  case TemporalStudy

  /** Repetition similarity between repeated viewings of an item. */
  case Repetition

  /** Densities sampled at fixation points against a template. */
  case PointSampling

  /** Template fitting and cross-validated evaluation. */
  case TemplateFit

  /** Least-squares decomposition of a map into predictor maps. */
  case Decomposition

  /** Scanpath and fixation-set comparison (MultiMatch, overlap, transport). */
  case ScanpathComparison

  /** The recipe family that carries this analysis, if it has one yet. */
  def family: Option[RecipeFamily] = this match
    case FixationStudy  => Some(RecipeFamily.FixationStudy)
    case EventRecording => Some(RecipeFamily.EventRecording)
    case TemporalStudy  => Some(RecipeFamily.TemporalStudy)
    case Repetition | PointSampling | TemplateFit | Decomposition | ScanpathComparison => None

/** A reusable part of other analyses, not an analysis of its own: it has a
  * descriptor and a schema but no family, cursor or archive role.
  */
enum AnalysisComponent derives CanEqual:
  /** Trial epochs: a measured anchor and observed coverage per trial. */
  case Epochs

object AnalysisKind:
  /** The analyses that do not yet have a recipe family, in declaration order. */
  val withoutFamily: Vector[AnalysisKind] =
    Vector(Repetition, PointSampling, TemplateFit, Decomposition, ScanpathComparison)

  /** The analysis each recipe family carries. */
  def of(family: RecipeFamily): AnalysisKind = family match
    case RecipeFamily.FixationStudy  => FixationStudy
    case RecipeFamily.EventRecording => EventRecording
    case RecipeFamily.TemporalStudy  => TemporalStudy
