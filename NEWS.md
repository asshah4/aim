# aim v0.0.0.9000

This is current developmental version. All features are still experimental.

## Next steps

- Create printing, summary, and extraction functions that are aligned with a hypothesis-driven project
- Consider inclusion of `equatiomatic` to improve fit/extract of formulas
-	Create a `collect_findings()` function to retrieve data more simply

## Features

As this is experimental, the functions may have sweeping changes. The major functions that are now available include:

- `project()` creates an object that holds multiple hypothesis
- `set_data()` adds a specific dataset to the core of the `project`
- `add_hypothesis()` adds pre-specified hypothesis and formulas for both inferential statistics and modeling, with the modeling defined by the `parsnip` model specifications
- `build_models()` adds the modeling analysis to the proposed hypothesis, and can be run all at once or in parts depending on need

The internals have been updated to focus on using mainly list objects, to allow for flexibility in object type.
