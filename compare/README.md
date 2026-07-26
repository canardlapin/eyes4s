# eyes4s-compare

Comparison: the Compare hierarchy, the shared alignment kernel, MultiMatch,
ScanMatch, CRQA, distribution measures, and optimal transport.

Each measure ships under the interface it ACTUALLY satisfies. Cosine
similarity is not a Metric; entropic Sinkhorn does not satisfy d(x, x) = 0.

CRQA retains its recurrence matrix alongside RR, DET, LAM, diagonal entropy,
trapping time, and L_max. Embedding dimension, delay, inclusive fixed or
target-rate radius selection, line minima, and the laminar axis are typed
configuration rather than ignored numeric arguments.

See `../PRD.md` for this module's requirements and `../.mote/` for its work
items.
