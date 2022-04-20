#' Hash a model or formula archetype
make_hash <- function(x, name = NA) {
  if (class(x)[1] == "model_archetype") {
    y <- vec_data(x)[-1]

    h <- paste0("NAME_", y$name, ".CLASS_md", ".TYPE_", y$type, ".SUB_", y$subtype, ".FORMULA_", field(y$fmls, "formula")) |>
      {
        \(.x) gsub(" ~ ", "__", .x)
      }() |>
      {
        \(.x) gsub(" \\+ ", "_", .x)
      }()
  }
  if (class(x)[1] == "formula_archetype") {
    y <- vec_data(x)
    h <- paste0("NAME_", name, ".CLASS_fmls", ".TYPE_", NA, ".SUB_", NA, ".FORMULA_", y$formula) |>
      {
        \(.x) gsub(" ~ ", "__", .x)
      }() |>
      {
        \(.x) gsub(" \\+ ", "_", .x)
      }()
  }

	# Return hash
	h
}
