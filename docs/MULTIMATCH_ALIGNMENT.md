# MultiMatch alignment convention

`MultiMatch.alignmentConvention` is
`MultiMatchAlignmentConvention.CanonicalOperandsExactCostTracebackV1`.
`MultiMatch.alignment(a, b)` exposes the selected movement correspondence,
with indices in the caller's operand order. `MultiMatch.compare` computes its
five components over that correspondence in canonical operand order.

The convention has four parts:

1. Check frame agreement and the existing minimum cardinality. Sort the operands
   by fixation count, then lexicographically by each ordered fixation's centre
   x, centre y and exact duration in microseconds. Floating coordinates use
   `Double.compare`, including its signed-zero ordering. Clock names, absolute
   onsets, source labels and object identity do not enter this order.
2. Use the existing monotone lattice: movement-vector Euclidean differences
   are cell costs, and the origin cell does not contribute to accumulated cost.
   The forward recurrence compares accumulated binary64 predecessor costs.
3. During traceback, prefer diagonal, then vertical, then horizontal when
   those predecessor costs compare exactly equal in canonical operand order.
   Near costs retain the smaller value. There is no epsilon band, decimal
   coordinate rounding or score tolerance in correspondence selection.
4. Reduce the selected pairs in that same canonical order. For a caller that
   supplied the operands in reverse, transpose the exposed indices. Reversal
   therefore retains the exact correspondence, cost and component values
   within a runtime. An unrepresentable accumulated cost is a named failure.

This is a convention for native ungrouped MultiMatch. It does not change general
`Alignment.dtw`, `Alignment.frechet` or their traceback policies. It also does
not substitute an average over tied paths or a reference library's undocumented
graph traversal order.

The independently authored public witness uses these pixel coordinates on a
1024 by 768 frame, 150ms onset spacing and 100ms fixation durations:

| Operand | Ordered fixation centres |
|---|---|
| A | (100,100), (100,103), (104,106) |
| B | (200,200), (201,205), (203,206), (208,206) |

A's movement vectors are (0,3) and (4,3). Both have squared distance 8 from
B's middle vector (2,1). Two optimal paths therefore have the same exact
radical cost, `sqrt(8) + sqrt(10)`, excluding the origin:

| Movement pairs, with zero-based indices | Direction | Length | Position |
|---|---:|---:|---:|
| (0,0), (0,1), (1,2), selected by this convention | 0.7951672353008665 | 0.9994031781074217 | 0.8862000024027900 |
| (0,0), (1,1), (1,2), also optimal | 0.9371670418109990 | 0.9983601410050056 | 0.8878559232713894 |

Shape and duration agree for the two choices. The different component values
are consequences of correspondence selection, and cannot be explained away
by widening a scalar tolerance. The recorded eyesim/igraph witness selected
the second path; eyes4s retains a versioned convention distinction instead of
declaring either optimal correspondence the uniquely correct answer.

The [independent oracle](../tools/multimatch-reference/generate.py) enumerates
every small monotone path without loading eyes4s. It retains exact rational
coordinates, exact squared-distance multisets and 80/120-digit radical sums.
Equal multisets prove the exact tie. Distinct expressions must have separated
decimal intervals; an unresolved oracle comparison is rejected. The five
component reductions use independent Python formulas with a named absolute
`1e-12` reference tolerance. That tolerance never participates in path selection.

Changing B's third x coordinate by `+2^-42` or `-2^-42` creates two exactly
representable near-tie fixtures. Their middle squared-cost difference is
`+2^-39` or `-2^-39`, respectively. They uniquely select opposite paths,
under this tiny coordinate perturbation. The direction component switches by
about 0.142. Borrowing a component-score tolerance to declare cost ties would
select the wrong correspondence. Both near fixtures and the exact tie run on
JVM and Scala.js.

The regression suite also covers a vertical/horizontal tie, all 729 paired
ternary movement sequences with unequal fixation durations, exact reversal,
identity, unrelated source clocks/onsets and named overflow failure.

`math.hypot` still supplies the movement costs. This convention specifies
selection for the actual binary64 cost matrix and does not promise that every
runtime or reference library computes identical near-tie costs. Cross-library
component agreement requires agreement on coordinates, grouping, cost
evaluation, accumulation and tie selection. The exact Gaussian pipeline
contract does not designate MultiMatch as an exact cross-runtime pipeline.

Regenerate and verify the public witnesses with:

```sh
python3 tools/multimatch-reference/generate.py --check
sbt 'compareJVM/testOnly *MultiMatchAlignmentSuite *MultiMatchConformanceSuite' \
    'compareJS/testOnly *MultiMatchAlignmentSuite *MultiMatchConformanceSuite'
```
