# Class ------------------------------------------------------------------------

#' Many models in a table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super class that combines both the `list` class
#' (and its derivative `list_of`) and regression models and/or hypothesis tests.
#' Models that are similar and share certain properties can be combined together
#' into a `md_tbl`.
#'
#' @name md_tbl
#' @importFrom dplyr mutate
#' @export
temporary <- function(..., data = NULL) {

	# Break early
	if (missing(..1)) {
		return(new_model_table())
	}

	# Get arguments and hammer them flat (retaining the names if needed)
	# Flatten arguments if possible to a simple list
	mtl <- hammer(...)
	nms <- names(mtl)

	# Now, everything should be an archetype object
	# Will need to "squish together" multiple tibbles for this
	tbl <- tibble()
	for (i in seq_along(mtl)) {
		x <- mtl[[i]]

		# Models
		if (class(x)[1] == "mdls") {
			fl <- field(x, "fmls")
			mi <- model_info(x)
			pe <- parameter_estimates(x)
			if (is.na(field(x, "strata_info"))) {
				si <- list(NA, NA, NA)
			} else {
				si <- as.formula(field(x, "strata_info"))
			}

			y <-
				x |>
				vec_data() |>
				tibble() |>
				# Add in formula components and corresponding roles
				dplyr::bind_cols(vec_data(fl)) |>
				dplyr::mutate(run = TRUE) |>
				dplyr::mutate(subname = dplyr::if_else(grepl(" ", name),
																							 gsub(" ", "", substr(name, 0, 7)),
																							 name)) |>
				dplyr::mutate(name = nms[i]) |>
				#dplyr::mutate(interaction = sapply(interaction, FUN = as.character)) |>
				dplyr::mutate(strata = as.character(si[[2]])) |>
				dplyr::mutate(level = as.character(si[[3]])) |>
				dplyr::select(-strata_info)
		}

		# Formulas
		if (class(x)[1] == "fmls") {
			# Ensure appropriate formula can be modeled later if need be
			f <- x[field(x, "order") %in% 1:3]

			# Formula hasn't been fit, so empty parameters
			pe <- parameter_estimates()
			mi <- model_info()

			# Expand formula into appropriate table
			y <-
				f |>
				vec_data() |>
				dplyr::bind_cols(fmls = f) |>
				tibble() |>
				# These items would need a model to be included
				mutate(
					model = list(NA),
					type = NA_character_,
					subtype = NA_character_,
					subname = NA_character_,
					description = NA_character_,
					level = NA,
					run = FALSE
				) |>
				dplyr::mutate(name = nms[i])
		}

		# Common elements
		z <-
			y |>
			dplyr::rowwise() |>
			mutate(across(
				c(outcome, exposure, mediator, strata, interaction),
				function(.x) {
					t <- .x
					if (length(t) == 0) {
						t <- NA_character_
					} else {
						t <- as.character(t)
					}
					t
				}
			)) |>
			# Generate list of runes for each row
			mutate(terms = list(get_runes(fmls))) |>
			dplyr::ungroup() |>
			mutate(terms = rune_list(terms))

		# Make individual row for a table
		tbl_row <- construct_model_table(
			model = z$model,
			type = z$type,
			subtype = z$subtype,
			name = z$name,
			subname = z$subname,
			number = z$number,
			description = z$description,
			formula = z$formulas,
			outcome = z$outcome,
			exposure = z$exposure,
			mediator = z$mediator,
			interaction = z$interaction,
			strata = z$strata,
			level = z$level,
			terms = z$terms,
			model_info = mi,
			parameter_estimates = pe,
			run = z$run
		)

		tbl <- dplyr::bind_rows(tbl, tbl_row)
	}

	# If data is given, then it should be tied to the object as well
	dl <- list()
	if (!is.null(data)) {
		nm <- deparse1(substitute(data))
		dl[[nm]] <- data
	}

	# Into the model md_tbl
	new_model_table(
		x = tbl,
		data_list = data_list(dl)
	)
}


#' @export
md_tbl <- function(...) {

	# Collect dots and validate
	dots <- rlang::list2(...)
	if (length(dots) == 0) {
		return(build_model_table(plan_model_table()))
	}
	# Handles either a list or a series of arugments
	if (length(dots) == 1 && is.list(dots[[1]])) {
		dots <- dots[[1]]
	}

	tbl <- plan_model_table()


}

#' @rdname md_tbl
#' @export
model_table <- md_tbl

#' Model table construction and definition
#' Work-horse function for reshaping model data into a table.
#' @keywords internal
#' @noRd
temporary_constructor <- function(model = list(),
																	type = character(),
																	subtype = character(),
																	name = character(),
																	subname = character(),
																	number = integer(),
																	description = character(),
																	formula = character(),
																	outcome = character(),
																	exposure = character(),
																	mediator = character(),
																	interaction = character(),
																	strata = character(),
																	level = numeric(),
																	terms = rune_list(),
																	model_info = model_info(),
																	parameter_estimates = parameter_estimates(),
																	run = logical()) {

	# Validation
	vec_assert(model, ptype = list())
	vec_assert(type, ptype = character())
	vec_assert(subtype, ptype = character())
	vec_assert(name, ptype = character())
	vec_assert(subname, ptype = character())
	vec_assert(number, ptype = integer())
	vec_assert(description, ptype = character())
	vec_assert(formula, ptype = character())
	vec_assert(outcome, ptype = character())
	vec_assert(exposure, ptype = character())
	vec_assert(mediator, ptype = character())
	vec_assert(interaction, ptype = character())
	vec_assert(strata, ptype = character())
	vec_assert(level, size = 1)
	vec_assert(terms, ptype = rune_list())
	vec_assert(parameter_estimates, ptype = parameter_estimates())
	vec_assert(model_info, ptype = model_info())
	vec_assert(run, ptype = logical())

	# Handle emptiness
	if (length(parameter_estimates) == 0) {
		parameter_estimates <- NA
	}
	if (length(model_info) == 0) {
		model_info <- NA
	}
	if (length(strata) == 0) {
		strata <- NA
	}
	if (length(interaction) == 0) {
		interaction <- NA
	}
	if (length(level) == 0) {
		level <- NA
	}

	# Essentially each row is made or added here
	tbl <- tibble::tibble(
		model = model,
		type = type,
		subtype = subtype,
		name = name,
		subname = subname,
		description = description,
		number = number,
		formula = formula,
		outcome = outcome,
		exposure = exposure,
		mediator = mediator,
		interaction = interaction,
		strata = strata,
		level = level,
		terms = terms,
		model_info = model_info,
		parameter_estimates = parameter_estimates,
		run = run
	)

	# Return tibble
	tbl
}

#' Model table construction
#' Work-horse for reshaping data into a table
#' @keywords internal
#' @noRd
structure_model_table <- function(x = tibble(),
																	model = character(),
																	formulas = fmls(),
																	data_info = list(),
																	model_fit = tibble(),
																	parameter_estimates = tibble(),
																	run = logical()) {

	stopifnot("Input must be a `tbl_df`" = is.tibble(x))

}


#' Model table planning
#' Sets up the tibble that will be used to make a `md_tbl`
#' @keywords internal
#' @noRd
plan_model_table <- function(model = character(),
														 formulas = fmls(),
														 data_info = list(),
														 model_fit = tibble(),
														 parameter_estimates = tibble(),
														 run = logical()) {

	# Essentially each row is made or added here
	# All will be empty, no validation at this step
	plan <-
		tibble::tibble(
			model = model,
			formulas = formulas,
			data_info = data_info,
			model_fit = list(model_fit),
			parameter_estimates = list(parameter_estimates),
			run = run
		)

	plan

}

#' Model table initialization
#' @keywords internal
#' @noRd
build_model_table <- function(x = tibble()) {

	# Validation
	stopifnot(is.data.frame(x))

	tibble::new_tibble(
		x,
		class = "md_tbl",
		nrow = nrow(x)
	)
}

#' @rdname md_tbl
#' @export
is_model_table <- function(x) {
	inherits(x, "md_tbl")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("md_tbl", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
print.md_tbl <- function(x, ...) {
	cat(sprintf("<%s>\n", class(x)[[1]]))
	cli::cat_line(format(x)[-1])
}

#' @export
vec_ptype_full.md_tbl <- function(x, ...) {
	"model_table"
}

#' @export
vec_ptype_abbr.md_tbl <- function(x, ...) {
	"md_tbl"
}


# Casting and coercion ---------------------------------------------------------

# SELF

#' @export
md_tbl_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
	out <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)


	new_md_tbl(out)
}

#' @export
md_tbl_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
	out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

	new_md_tbl(out)
}

#' @export
vec_ptype2.md_tbl.md_tbl <- function(x, y, ...) {
	md_tbl_ptype2(x, y, ...)
}

#' @export
vec_cast.md_tbl.md_tbl <- function(x, to, ...) {
	md_tbl_cast(x, to, ...)
}

# TIBBLE

#' @export
vec_ptype2.md_tbl.tbl_df <- function(x, y, ...) {
	md_tbl_ptype2(x, y, ...)
}

#' @export
vec_ptype2.tbl_df.md_tbl <- function(x, y, ...) {
	md_tbl_ptype2(x, y, ...)
}

#' @export
vec_cast.md_tbl.tbl_df <- function(x, to, ...) {
	tib_cast(x, to, ...)
}

#' @export
vec_cast.tbl_df.md_tbl <- function(x, to, ...) {
	tib_cast(x, to, ...)
}

# DATA.FRAME

#' @export
vec_ptype2.md_tbl.data.frame <- function(x, y, ...) {
	md_tbl_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.frame.md_tbl <- function(x, y, ...) {
	md_tbl_ptype2(x, y, ...)
}

#' @export
vec_cast.md_tbl.data.frame <- function(x, to, ...) {
	df_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.md_tbl <- function(x, to, ...) {
	df_cast(x, to, ...)
}
