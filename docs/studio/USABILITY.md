# S10.8 researcher usability session

**Status: protocol prepared; no researcher sessions completed.** Five independent
researchers must use the same qualified candidate. This document prepares the
session; screenshots, automated checks and an agent rehearsal do not count as
human observations. The owner arranges participants; no invitations are sent by
this workflow.

## Freeze the session packet

Record source SHA/tree, package ZIP hash, architecture, OS build, display scale,
input devices and the package check receipt. Use a writable copy of the bundled
example, with its source hash, and retain the pre-session project. Private study
data are unnecessary for this usability check. Give participants only the task
sheet below, not the moderator key. Archive per-session saved projects, exports,
consented screen captures, timestamps and exact help given outside Git.

Use five researchers who did not implement the application. Record prior
experience with eye tracking and similar software. Allow 45 minutes each; use
the same task order and wording. The moderator may resolve an OS permission or
installation problem, but must record it. Do not teach the scientific answer or
quietly fix the project. Mark a task assisted if guidance beyond its wording was
needed. Missing or interrupted sessions stay missing and are replaced openly.

## Participant task sheet

1. Open the example as your own project. Identify its fixation and trial sources,
   the current dataset revision and any records/trials that were not admitted.
   Explain what the application retained and what will enter an analysis.
2. Find one retrieval query in Compare. Explain which encoding trial is its match,
   which trials are controls, and why each belongs in that role. Inspect the
   source records for a point or row that supports your explanation.
3. In Analysis, change a Gaussian scale in a draft. Explain whether the displayed
   result already uses that change. Apply/save/run through the normal controls;
   wait for completion and identify the resulting revision and run.
4. Inspect a matched-minus-control result at a selected scale. Explain what its
   sign and magnitude mean, which trials contribute, and what cannot be concluded
   from that value alone. Find the per-query result and its supporting pairs.
5. Save, quit and reopen the project. Confirm the same run and inputs, export a
   figure and result bundle, and locate the recipe/source identities in the export.
   Report anything clipped, unreadable, ambiguous or unexpectedly changed.

At the end ask: “What was hardest to understand?” and “What would you need before
using this for your own study?” Record answers verbatim before discussing fixes.

## Moderator answer key and scoring (not given to participants)

Use the **actual saved recipe, admission ledger and result** as the key, not a
historical board count. The example initially represents t2: data r3, saved rev 4,
run 7, and an unrun rev 5 draft. New executions create their own identifiers.
The synthetic fixture includes quarantine, absent trials and missing images
intentionally; these are not evidence of real study quality.

Score the following three concepts independently as correct/incorrect/unobserved,
with the participant's words and the UI artifact supporting the judgment:

- **Comparison:** identifies the selected query and true match, describes the
  configured eligible control pool and exclusions, and does not call a lure's
  expected absence of a study match a successful identity match. The operative
  pool is the served resolved design, not an assumed “all other trials” pool.
- **Exclusion and freshness:** distinguishes retained source rows from admitted
  trials and actual contributing queries; locates named exclusion/failure reasons;
  recognizes that a draft does not retroactively change an existing saved run.
- **Meaning:** describes matched similarity minus the configured control mean at
  the selected scale, with the metric's orientation and units; distinguishes a
  per-query contrast from aggregation; does not infer accuracy, significance or
  memory causation from a positive value alone. They can locate its paired values.

A researcher passes the comprehension criterion only when **all three** concepts
are correct without teaching. Record task completion, assistance, time and export
identity separately; a correct coached answer is not an unassisted pass.

S10.8 acceptance requires at least **4 of 5** researchers to pass comprehension.
Also triage every observed defect: loss/corruption, misleading scientific identity
or an inaccessible essential action blocks release regardless of the count.
Less severe problems receive motes with reproduction, candidate SHA and evidence.
Do not change the rule after seeing results; an amended protocol starts a new
round and retains the earlier outcome.

## Recording template

For each researcher R1–R5 retain: session date, candidate identifiers, environment,
experience, task start/end, completed/assisted/not-run, exact interpretation,
comparison/exclusion/meaning scores, moderator interventions, project/export
hashes, capture paths and defect mote IDs. Leave scores null until observed.

The release packet in `docs/studio/evidence/20261027-release/manual-checks.json`
starts with five empty slots and explicit not-run accessibility/installation
checks. Complete it only from actual observations. See [PACKAGING.md](PACKAGING.md)
for installation and [A11Y_AUDIT.md](A11Y_AUDIT.md) for the VoiceOver script.
