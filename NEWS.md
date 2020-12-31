# octomod v0.0.0.9000

This is current developmental version. All features are still experimental.

## Next steps

-   Create printing, summary, and extraction functions that are aligned with a hypothesis-driven project
-   Consider inclusion of `equatiomatic` to improve fit/extract of formulas

## Features

As this is experimental, the functions may have sweeping changes. The major functions that are now available include:

- `octomod()` creates an object that holds multiple hypothesis
- `core()` adds a specific dataset to the core of the `octomod`
- `arm()` adds pre-specified hypothesis and formulas for both inferential statistics and modeling, with the modeling defined by the `parsnip` model specifications
- `equip()` adds the modeling analysis to the proposed hypothesis, and can be run all at once or in parts depending on need
- `sharpen()` adds model-level information about a `parsnip` model specification, but currently only works for logistic regressions
