# Manual extensions

This directory vendors the reviewed extensions used by the current
`prospectorKiribatiBioMarkers` exemplar:

- Quarto Manual v0.1.0;
- Quarto-emit v1.0.0.

The default `authoring` profile renders documentation without file emission.
The explicit `emit` profile loads Quarto-emit and sets `QUARTO_EMIT=1`.
Both profiles disable computation; scientific work runs through the generated
container runner and targets entry point.
