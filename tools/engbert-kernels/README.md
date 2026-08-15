# Engbert-Kliegl velocity oracle

`reference.json` pins the interior five-point `vecvel` output from the University of Potsdam
Python translation of the Engbert Microsaccade Toolbox at commit
`a3eba6e9f1464c953c81fbd87944ba7678c2cf64`.

The oracle's scope is deliberately narrow. `EngbertKlieglConformanceSuite`
also checks a complete candidate using a radius produced by the pinned
toolbox. The algorithm card records the remaining deliberate differences:
eyes4s omits the toolbox's three-point boundary velocities, uses the
publication's median-square threshold definition, includes the final
candidate velocity when reporting the peak, and breaks candidates at invalid
observations.

Regenerate it from the pinned upstream source with:

```sh
python3 tools/engbert-kernels/generate_reference.py
```

The generator prints JSON to standard output so regeneration never overwrites
the reviewed fixture implicitly. `EngbertKlieglConformanceSuite` embeds the
same values for deterministic JVM and JavaScript checks without network or
Python at test time.
