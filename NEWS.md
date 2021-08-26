# dagger v0.0.0.9000

This is current developmental version. 

## Next Steps

- Method for incorporation of other testing mechanisms (e.g. `lm()` and `t.test()`)
- Development of path-based functions
- Addition of confounding measurements based on hypothesis
- Regeneration of hypotheses and functions

## Features

- `hypothesize()` creates individual hypotheses to be tested
- `study()` initializes a data structure for mapping on `hypothesis` objects
- `draw_hypothesis()` composes a hypothesis onto a `study` object
- `construct_models()` and `construct_paths()` create the corresponding component tables underlying the `study` object
- `extract_models()` returns the tidy and raw versions of the 

