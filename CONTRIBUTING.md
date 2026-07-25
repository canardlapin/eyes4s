# Contributing

Thanks for your interest. `eyes4s` is pre-alpha: the architecture (`eyes4s.md`)
and requirements (`PRD.md`) are settled, and the kernel is being built against
them.

## Before opening a pull request

Run what CI runs:

```sh
sbt headerCheckAll scalafmtCheckAll scalafmtSbtCheck githubWorkflowCheck
sbt testAll checkBoundaries
```

## The five rules

`AGENTS.md` carries the full design contract. These are the ones that most often
decide whether a change is accepted:

1. **Parse, don't validate.** Private constructors, `Either`-returning smart
   constructors. An invariant belongs to the type.
2. **Failure is a value, and it names its operands.** No `null`, no sentinel, no
   throwing from a pure path, no silently changing a caller's row count. Error
   cases say *which* object failed, not only why.
3. **State conventions in types.** If a reader has to consult a comment to know
   whether an interval is half-open or which way the y-axis runs, the design is
   not finished.
4. **A measure ships under the interface it satisfies.** Cosine similarity is not
   a metric. Do not claim laws you have not tested.
5. **New law suites are verified by mutation.** Break the implementation on
   purpose and confirm the suite fails. A law suite that passes vacuously is
   worse than none, because it looks like evidence.

## Boundaries

Two invariants are enforced mechanically, and a PR that violates either will fail
CI rather than review:

- `eyes4s-kernel` contains no ocular vocabulary (`checkKernelPurity`).
- No pure module acquires an effect system (`checkModuleBoundaries`).

If `checkKernelPurity` rejects a name, consider that it may be pointing at a real
layering mistake before reaching for a synonym.

## Scope

`eyes4s` is deliberately not a statistics package, a plotting library, a vendor
SDK binding, or a tensor library. See `PRD.md` §Non-Goals before proposing a
feature — a rejection there is a decision, not an oversight.

## Sign-off

Commits must be signed off under the [Developer Certificate of
Origin](https://developercertificate.org/):

```sh
git commit -s
```
