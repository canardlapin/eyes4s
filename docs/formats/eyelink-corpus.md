# EyeLink conformance corpus

The EyeLink corpus is a versioned evidence inventory, not a folder of example
files. Its manifest distinguishes three scientifically different things:

1. project-generated ASC fixtures containing no human data;
2. real, deidentified ASC files for which redistribution permission has been
   documented; and
3. private real-device EDF/ASC pairs that remain outside the repository and are
   represented only by digests and publishable aggregate results.

The current corpus version is `2026.08.15.3`. It is bound to EyeLink support
contract `0.6-draft.1`. The manifest intentionally contains planned real-data
rows: there are no real-device bytes or digests in the public corpus yet.
Consequently the synthetic fixtures exercise parser semantics, but they do not
validate a hardware family, an EDF2ASC executable, or a released support claim.

## Synthetic license

Files whose manifest kind is `synthetic` were written for eyes4s, contain no
measurements from a human or animal, and are distributed under the repository's
Apache License 2.0. Each file says this on its first line. Synthetic files may
never claim a hardware capability or `converter-receipt` evidence.

## Admission rules

No real fixture enters the repository until a reviewer records all of the
following in the manifest:

- the source and scope of acquisition or redistribution permission;
- the privacy/deidentification disposition;
- tracker family and firmware when known;
- sampling rate, eye layout, tracking mode, and native coordinate modes;
- the ASC SHA-256 digest;
- the EDF SHA-256 digest when retention and disclosure are permitted;
- the EDF2ASC executable SHA-256, reported version, exact scientific option
  vector, platform, and normal/failsafe mode; and
- the support-matrix capabilities the fixture is intended to test.

An included real file requires explicit redistribution permission and a
deidentification review. A private pair is never assigned a repository path;
it is addressed by EDF and ASC digests and may contribute only aggregate
conformance results. A planned acquisition carries no path, digest, or
converter receipt and therefore cannot be mistaken for observed evidence.

Public CI reads only manifest rows marked `included`. It does not download
private data, invoke EDF2ASC, or attempt to locate digest-only files.

## Review and versioning

The canonical manifest is
`io/src/test/resources/eyes4s/io/eyelink/corpus/manifest.tsv`. Every included
file is content-addressed by SHA-256. The parser renders a deterministic
canonical TSV representation, and the JVM corpus court verifies that the
checked-in manifest is already canonical and that every local digest matches.

Any fixture addition or byte change requires:

1. a corpus-version increment;
2. permission/privacy review before the bytes are added;
3. an updated digest and capability list;
4. review of capability and pairwise-dimension coverage; and
5. later oracle, JVM/Scala.js parity, mutation, and validation-artifact courts.

Coverage has four explicit states: missing, planned only, synthetic only, and
real evidence available. Neither a planned row nor a synthetic fixture is
reported as real evidence. Missing and non-real coverage remains visible in the
generated validation report.

## Hardware acquisition targets

The planned rates follow vendor specifications rather than inference. SR
Research documents 250, 500, 1000, and 2000 Hz modes for the EyeLink 1000 Plus
and Portable Duo, with remote/head-free configurations limited to 1000 Hz in
the cited specifications. EyeLink 3 is advertised at up to 1000 Hz. These are
acquisition targets only; eyes4s will not advertise device support until
corresponding real-device evidence passes the full conformance court.

Primary vendor sources:

- [EyeLink ASC file analysis documentation](https://www.sr-research.com/download/dispdoc/page25.html)
- [EyeLink data types](https://www.sr-research.com/download/dispdoc/page9.html)
- [EyeLink 1000 Plus technical specifications](https://www.sr-research.com/eyelink-1000-plus-technical-specifications/)
- [EyeLink Portable Duo technical brochure](https://www.sr-research.com/wp-content/uploads/2021/06/eyelink-portable-duo-brochure.pdf)
- [SR Research current product information](https://www.sr-research.com/)

The vendor specifications note that available modes can depend on purchased
options. The eventual fixture manifest must record the observed configuration;
the hardware family name alone is never sufficient evidence.
