#' Hash a model or formula archetype
make_hash <- function(x, name = NA) {
  if (class(x)[1] == "model_archetype") {
    y <- vec_data(x)[-1]

    h <- paste0("NAME_", y$name, "_CLASS_md", "_TYPE_", y$type, "_SUB_", y$subtype, "_FORMULA_", field(y$fmls, "formula")) |>
      {
        \(.x) gsub(" ~ ", "_", .x)
      }() |>
      {
        \(.x) gsub(" \\+ ", "_", .x)
      }()
  }
  if (class(x)[1] == "formula_archetype") {
    y <- vec_data(x)
    h <- paste0("NAME_", name, "_CLASS_fmls", "_TYPE_", NA, "_SUB_", NA, "_FORMULA_", y$formula) |>
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
