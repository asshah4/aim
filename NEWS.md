# octomod v0.0.0.9000

This is current developmental version. All features are still experimental.

## Next steps

* Create a `summary()` and `print()` functions for the `octomod`, such that information can easily be extracted about the scope of a project

## Features

As this is experimental, the functions may have sweeping changes. The major functions that are now available include:

* `octomod()` creates an object that holds multiple hypothesis
* `add_core()` adds a specific dataset to the core of the `octomod`
* `add_arm()` adds pre-specified hypothesis and formulas for both inferential statistics and modeling, with the modeling defined by the `parsnip` model specifications
* `add_outfit()` adds the modeling analysis to the proposed hypothesis, and can be run all at once or in parts depending on need
