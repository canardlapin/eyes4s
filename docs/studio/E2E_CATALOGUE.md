# Studio E2E catalogue

The catalogue binds E2E-02 through E2E-25 to named feature-boundary acceptance tests.
`python3 tools/studio-e2e/check.py` checks this checkout’s XML reports after `sbt studioAll`,
refuses missing, failed, errored or skipped cases, and records source/report hashes and the candidate tree.
Core and app cases run on JVM and Scala.js; native backend cases and shell cases name their actual platform.

The native command and golden-command journeys additionally cover import/admission, preparation,
execution, result/source readback, reporting, durable native archives, reopen and export against the
library. This is the real-backend regression route accepted under S3.7, alongside the explicit native
cases below. A feature-boundary test is not represented as a repeated complete UI journey on every backend.

| Scenario | Acceptance | Executable evidence |
|---|---|---|
| E2E-02 | First run and bundled example | [DataFirstRunFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/data/DataFirstRunFxSuite.scala) (native shell); [ParityChecklistSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/shell/ParityChecklistSuite.scala) (native shell) |
| E2E-03 | Linked selection | [ExploreLinkedSelectionFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/explore/ExploreLinkedSelectionFxSuite.scala) (native shell) |
| E2E-04 | Draft → Save & run → stale bindings | [DraftLifecycleSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/analysis/DraftLifecycleSuite.scala) (native shell) |
| E2E-05 | Executed design equals prepared preview | [ResolvedDesignFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/analysis/ResolvedDesignFxSuite.scala) (native shell); [PreviewSnapshotSuite](../../studio/core/src/test/scala/eyes4s/studio/core/fixture/PreviewSnapshotSuite.scala) (portable core JVM+JS) |
| E2E-06 | Cancel mid-run | [ExecutionServiceSuite](../../studio/core/src/test/scala/eyes4s/studio/core/execution/ExecutionServiceSuite.scala) (portable core JVM+JS) |
| E2E-07 | Reject stale completion | [ExecutionServiceSuite](../../studio/core/src/test/scala/eyes4s/studio/core/execution/ExecutionServiceSuite.scala) (portable core JVM+JS) |
| E2E-08 | Running results never replace shown results | [RunInProgressFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/shell/RunInProgressFxSuite.scala) (native shell) |
| E2E-09 | Reporting reuses scores and keeps control pool | [ReportingEditorSuite](../../studio/app/src/test/scala/eyes4s/studio/app/compare/ReportingEditorSuite.scala) (portable app JVM+JS); [RealStudyBackendSuite](../../studio/core/.jvm/src/test/scala/eyes4s/studio/core/real/RealStudyBackendSuite.scala) (native backend JVM) |
| E2E-10 | Underlay disclosure survives export | [FigureExportSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/figures/FigureExportSuite.scala) (native shell) |
| E2E-11 | Missing asset repair | [SourcesFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/data/SourcesFxSuite.scala) (native shell); [SourceRepairFileSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/data/SourceRepairFileSuite.scala) (native shell) |
| E2E-12 | Crash recovery | [RecoverySuite](../../studio/core/src/test/scala/eyes4s/studio/core/session/RecoverySuite.scala) (portable core JVM+JS) |
| E2E-13 | Failed run exposes diagnostics | [ExecutionServiceSuite](../../studio/core/src/test/scala/eyes4s/studio/core/execution/ExecutionServiceSuite.scala) (portable core JVM+JS) |
| E2E-14 | Blocked preflight and occurrence remedy | [PreflightFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/analysis/PreflightFxSuite.scala) (native shell); [AdversarialScienceSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/runtime/AdversarialScienceSuite.scala) (native shell) |
| E2E-15 | Discard draft | [DraftLifecycleSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/analysis/DraftLifecycleSuite.scala) (native shell) |
| E2E-16 | Rebind or keep stale figure | [FigureBindingSuite](../../studio/app/src/test/scala/eyes4s/studio/app/figures/FigureBindingSuite.scala) (portable app JVM+JS) |
| E2E-17 | Crumb round-trip into Explore and back | [ContextStripFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/shell/ContextStripFxSuite.scala) (native shell) |
| E2E-18 | Layout persistence and pop-out retention | [LayoutPersistenceFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/dock/LayoutPersistenceFxSuite.scala) (native shell); [ProjectLifecycleFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/project/ProjectLifecycleFxSuite.scala) (native shell) |
| E2E-19 | Second writer refusal | [AtomicSaveFaultSuite](../../studio/core/src/test/scala/eyes4s/studio/core/session/AtomicSaveFaultSuite.scala) (portable core JVM+JS) |
| E2E-20 | Changed source blocks use and can be repaired | [SourceChecksFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/shell/SourceChecksFxSuite.scala) (native shell); [SourceRepairFileSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/data/SourceRepairFileSuite.scala) (native shell) |
| E2E-21 | Recognition by-design category | [PresetSuite](../../studio/app/src/test/scala/eyes4s/studio/app/analysis/PresetSuite.scala) (portable app JVM+JS); [RealPreviewCursorSuite](../../studio/core/src/test/scala/eyes4s/studio/core/real/RealPreviewCursorSuite.scala) (portable core JVM+JS) |
| E2E-22 | Perception → Imagery blank display | [AdversarialScienceSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/runtime/AdversarialScienceSuite.scala) (native shell); [FigureExportSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/figures/FigureExportSuite.scala) (native shell) |
| E2E-23 | Minimum-query toggle discloses dropped cells | [ReportingEditorSuite](../../studio/app/src/test/scala/eyes4s/studio/app/compare/ReportingEditorSuite.scala) (portable app JVM+JS); [RealStudyBackendSuite](../../studio/core/.jvm/src/test/scala/eyes4s/studio/core/real/RealStudyBackendSuite.scala) (native backend JVM) |
| E2E-24 | Dark theme journey | [ThemeFxSuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/shell/ThemeFxSuite.scala) (native shell) |
| E2E-25 | Keyboard-only golden journey | [KeyboardJourneySuite](../../studio/desktop/src/test/scala/eyes4s/studio/desktop/journey/KeyboardJourneySuite.scala) (native shell) |

The Recognition and imagery mini-fixtures are committed executable specimens in `PresetSuite`,
`RealPreviewCursorSuite` and `AdversarialScienceSuite`. They test explicit by-design declarations,
item-ID collisions, blank displays and distinct controls; they contain no private-study exports.

The Linux Studio workflow runs the catalogue checker and its refusal tests after Studio tests,
and uploads the resulting receipt alongside the screenshots and reports.

Qualification passed on 2026-10-21 for `356db27e` (tree `6b59ebe3`). The
[qualification receipt](evidence/20261021-g4/qualification.json) binds the full canonical and
strict Studio gates, local/hosted catalogue reports, documentation consumer and native output-scale-2 evidence.
All 24 scenarios passed on their required platforms; 4,343 portable and 797 local desktop tests passed,
with one private-data test opt-in and ignored. Hosted Linux, GTK2, macOS and clean-clone jobs passed.
A source reference alone does not qualify a case. G2 acquisition/timing, all-board visual sign-off,
VoiceOver, the performance court, packaging and researcher usability remain separate acceptance obligations.
