# path v0.1.0

This is current developmental version. 

## Next Steps

- Method for incorporation of other testing mechanisms (e.g. `lm()` and `t.test()`)
- Development of path-based functions
- Addition of confounding measurements based on hypothesis
- Regeneration of hypotheses and functions

## Features

These functions are currently stable, and should continue to have similar inputs and outputs. Over time, there should be expansion in their use.

- `hypothesize()` creates individual hypotheses to be tested
- `create_study()` initializes a data structure for mapping on `hypothesis` objects
- `add_hypothesis()` adds to an existing `study` object
- `construct_map()` creates the corresponding component tables underlying the `study` object
- `extract_models()` returns the tidy and raw versions of the analyzed `study`

