# eyes4s-aoi

Areas of interest: `AoiSet`, membership policies (`Multiple`, `ExclusiveByPriority`,
`SmallestContaining`, `RejectOverlap`), dwell, first-entry latency, run counts, transition
counts, and an exact time ledger (`AoiAssignmentReport`, `accountingHolds`).

The accounting is stated as laws in `eyes4s-laws` (`AoiLaws.accounting`, checked by
`AoiLawsSuite` with mutation receipts): dwell partitions the fixation time with the background
under a single-membership policy and sums to union plus duplicated time under `Multiple`; first
entry is defined exactly when dwell is positive and is the onset of the first contained fixation;
run count is the number of maximal abutting blocks of contained fixations; `RejectOverlap` refuses
exactly when a fixation lies in two areas; and all of it is invariant under a translation of
scanpath and areas.

See `../PRD.md` for this module's requirements and `../.mote/` for its work items.
