# Validated domain products and construction mirrors

Private case-class constructors in Scala 3.7.4 do not prevent a consumer from summoning a public `Mirror.ProductOf` and calling `fromProduct` with invalid fields. A validated type must prevent this route as well as unchecked constructors and copies.

The library migration uses sealed abstract case classes with private constructors and companion-private concrete factories. Existing package-trusted constructors retain their original visibility. The existing public smart constructors remain the only public construction path. Fields, extractors, equality, hashing, string representation and read-only `Product` inspection remain available. The convention is recorded by bead `q-domain-constructing-mirrors`.

A constructing product mirror is deliberately absent. Decoders reconstruct through smart constructors; code that reads fields uses accessors or existing read-only product inspection. Generic construction derivation is not an alternative admission path.

The kernel conformance suite pairs each negative mirror probe with positive compilation of the same fully qualified public type. Behavioral checks cover valid construction, rejection of invalid dimensions, equality, hash behavior, extractors, frame retargeting and structural specifications. Direct constructor and unchecked-copy probes also require compile-time rejection. A representation mutant restores the old final case class and ordinary allocation: the mirror probes must fail after test classes are rebuilt on each runtime.

The library sweep covers 121 formerly concrete private-constructor products: 27 in kernel and 94 across core, surface, AOI, compare, design, plan, results, codec and IO. Every name has a positive external-consumer compilation probe and a negative constructing-mirror probe on JVM and Scala.js. A second representation mutant restores the original `DataRecord` escape, and must fail its consumer test on both runtimes after rebuilding test classes.

Trusted internal updates formerly written as case-class copies now use the same private factory. Read-only derivations do not regain construction authority: `TrialKey` and `TrialIdentity` project their fields into the former tuple digest derivation, with direct comparisons against legacy product-derived digests. Artifact-reference diagnostics project the digest field explicitly, with a legacy diagnostic-shape comparison. Diagnostic test examples construct valid artifact identities through their public smart constructors.

This removes a public generic construction route deliberately. Consumer decoders and generic derivations that depended on `Mirror.ProductOf` must use the public smart constructor or a checked codec. Read-only `Product` access remains supported; arbitrary reflective construction is outside this API contract.

The Studio core slice protects a further 108 products, including saved dataset declarations, archive indexes, native budgets, preview state and nominal source identities. Its 109 external-consumer checks cover constructing mirrors and private copy helpers. Nine private copy helpers preserve trusted internal state updates; inaccessible abstract constructors no longer carry unused defaults, while smart constructors and private factories retain their defaults. The original native-budget representation is restored as a mutant and caught on both runtimes.

Complete Studio core qualification passes 1,089 JVM tests and 1,001 Scala.js tests with strict compiler warnings, a 4 GiB sbt heap and six active processors. The existing test timeouts remain unchanged. An earlier run under competing builds hit three asynchronous backend timeouts; those suites passed in isolation, followed by the complete passing matrix. Both runs are retained in the evidence receipt.

The remaining 50 private products in Studio app, visualization and desktop are a separate unfinished slice under the parent invariant-enforcement ticket. These library and Studio core slices do not claim house-wide completion.
