# Validated domain products and construction mirrors

Private case-class constructors in Scala 3.7.4 do not prevent a consumer from summoning a public `Mirror.ProductOf` and calling `fromProduct` with invalid fields. A validated type must prevent this route as well as unchecked constructors and copies.

The kernel migration uses sealed abstract case classes with private constructors and companion-private concrete factories. The existing public smart constructors remain the only public construction path. Fields, extractors, equality, hashing, string representation and read-only `Product` inspection remain available. The convention is recorded by bead `q-domain-constructing-mirrors`.

A constructing product mirror is deliberately absent. Decoders reconstruct through smart constructors; code that reads fields uses accessors or existing read-only product inspection. Generic construction derivation is not an alternative admission path.

The kernel conformance suite pairs each negative mirror probe with positive compilation of the same fully qualified public type. Behavioral checks cover valid construction, rejection of invalid dimensions, equality, hash behavior, extractors, frame retargeting and structural specifications. Direct constructor and unchecked-copy probes also require compile-time rejection. A representation mutant restores the old final case class and ordinary allocation: the mirror probes must fail after test classes are rebuilt on each runtime.

This is the kernel slice. Other modules remain tracked by the parent invariant-enforcement ticket; this document does not claim their migration is complete.
