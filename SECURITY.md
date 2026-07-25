# Security Policy

## Scope

`eyes4s` is a pure analysis library. It parses data files, which is its main
exposure: a malformed recording should produce a diagnostic, never a crash,
an unbounded allocation, or arbitrary code execution.

## Reporting

Report vulnerabilities privately through GitHub's "Report a vulnerability"
button on the Security tab of this repository. Please do not open a public
issue first.

Include the input that triggers the problem where you can. Parser inputs are
the most valuable reports.

## Supported versions

Pre-1.0: only the latest release is supported.
