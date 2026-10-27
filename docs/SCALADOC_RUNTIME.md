# Scaladoc runtime qualification

The 2026-10-27 all-module JVM Scaladoc run passed on Temurin **25.0.4.1**, with
Scala 3.7.4 and no `TieredStopAtLevel` workaround. It ran in a fresh standalone
clone with no pre-existing API output, using `sbt rootJVM/doc`; all library source
files match the qualified main at `01628cbd441ad5de1599adf3c43a0d129ace2a6f`.
The [receipt](evidence/release-20261027/scaladoc.json) retains the command, elapsed
time and runtime, and its compressed log preserves warnings as well as success.

The earlier all-module failure on JDK 25.0.1 remains historical negative evidence.
A successful run on 25.0.4.1 does not identify its mechanism or establish that a
particular upstream patch fixed it. No compiler upgrade or broad suppression was
introduced to obtain this result.

For the library release, JVM 17 and 21 remain the supported CI documentation
routes. The qualified main's [hosted Checks receipt](studio/evidence/20261026-hardening/qualification/hosted/checks-workflow.json)
records those matrix results; the next candidate must pass its own checks.
Studio needs JDK 25, kept in `~/Library/Java/JavaVirtualMachines`, as described in
`AGENTS.md`. Do not use a temporary JDK installation for a long gate.

The isolated artifact consumer's existing `-J-XX:TieredStopAtLevel=1` setting has a
narrower scope and remains unchanged. This documentation pass is not evidence to
remove a workaround from a different execution path without testing that path.
