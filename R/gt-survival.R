#' Make complex regression table
#'
#' @param object A `mdl_tbl` object with survival-based regression outcomes
#'
#' @param data A dataset that corresponds to the model group of interest, and
#'   includes survival times.
#'
#' @param events Is a formula or list of formulas selecting the outcome
#'   variables of interest. The LHS is always the name of the variable that will
#'   be selected. The RHS is the potential label for the output table. If no
#'   label is desired, can place an `NA` value on RHS of formula.
#'
#' @param followup Character vector naming the followup duration variable. Must
#'   be either same length as __events__ or be of length of 1 (which will be
#'   recycled).
#'
#' @param terms Is a formula or list of formulas selecting the model terms that
#'   should be used. The LHS is always the name of the variable that will be
#'   selected. The RHS is the potential label for the output table. If no label
#'   is desired, can place an `NA` value on RHS of formula.
#'
#' @param rate_difference If there are only two levels in the term, the rate
#'   difference between the levels will be calculated. Defaults to `FALSE`.
#'   Presumes a 95% confidence interval as the default.
#'
#' @import gt
#' @export
tbl_categorical_hazard <- function(object,
																	 data,
																	 events = formula(),
																	 followup = character(),
																	 terms = formula(),
																	 adjustment = formula(),
																	 rate_difference = TRUE,
																	 ...) {

	# Ensure only one model family is present
	if (length(unique(object$name)) > 1) {
		stop('Cannot combine models from different datasets or regressions into a table safely.')
	}

	# Get relevant filtering variables
	## Outcomes
	out <- formulas_to_named_list(events)
	out_nms <- names(out)
	out_lab <- unlist(unname(out))
	## Terms
	tms <- formulas_to_named_list(terms)
	tms_nms <- names(tms)
	tms_lab <- unlist(unname(tms))
	## Adjustment
	lvl <- formulas_to_named_list(adjustment)
	lvl_nms <- names(lvl)
	lvl_lab <- unlist(unname(lvl))

	# Create subset of model_table
	obj <-
		object |>
		dplyr::filter(grepl(paste0(out_nms, collapse = '|'), outcome)) |>
		flatten_table() |>
		dplyr::filter(grepl(paste0(tms_nms, collapse = '|'), term)) |>
		dplyr::select(number, outcome, term, estimate, conf_low, conf_high, p_value)

	# Check early to see if terms are in the appropriate adjustment sets
	for (t in tms_nms) {

		possible_terms <- obj[obj$number == as.numeric(lvl_nms[1]),]$term

		if (!any(grepl(t, possible_terms))) {
			stop(
				'Adjustment sets do not contain the term `',
				t,
				'`. Please reselect terms or adjustment set levels.'
			)
		}

	}

	# Incidence rates
	dat <- data[c(out_nms, followup, tms_nms)]

	# Number of tables to make depends on...
	#		Number of outcomes
	# 	Number of terms

	# Get tables ready to be stored

	tbl_tms <- list()
	tbl_out <- list()

	for (t in tms_nms) {
		for (o in out_nms) {

			# Adjusted hazards first
			adj <-
				obj |>
				dplyr::filter(grepl(t, term)) |>
				dplyr::filter(grepl(o, outcome)) |>
				tidyr::pivot_wider(
					names_from = term,
					values_from = c(estimate, conf_low, conf_high, p_value),
					names_glue = '{term}_{.value}'
				) |>
				dplyr::filter(number %in% as.numeric(lvl_nms)) |>
				dplyr::mutate(outcome = o) |>
				tibble::add_row(number = 0, outcome = o, .before = 1)

			# Person years into table
			py <-
				survival::pyears(survival::Surv(dat[[followup]], dat[[o]]) ~ dat[[t]])

			py$pyears <- py$pyears / 100

			rates <-
				rbind(n = py$n, events = py$event, risk = py$event / py$pyears) |>
				as.data.frame() |>
				rownames_to_column(var = 'label')

			# Save number per group for later in the table
			n <- rates[1, -1]

			# Add in rate differences if needed
			if (rate_difference & length(levels(dat[[t]]) == 2)) {

				# Number of Disease in Exposed & Controls
				nde <- py$event[1]
				ndc <- py$event[2]
				nd <- nde + ndc

				# Person-Time in Exposed & Controls
				pte <- py$pyears[1]
				ptc <- py$pyears[2]
				pt <- pte + ptc

				# Incidence Rate in Exposded & Controls
				ire <- nde/pte
				irc <- ndc/ptc
				ir <- nd/pt

				# Estimates & Confidence Intervals (95%)
				est <- ire - irc
				cl <- est - qnorm(0.9725) * sqrt(nde / pte^2 + ndc / ptc^2)
				ch <- est + qnorm(0.9725) * sqrt(nde / pte^2 + ndc / ptc^2)

				# Add buffering rows
				rd <-
					dplyr::bind_cols(rd_est = est, rd_conf_low = cl, rd_conf_high = ch) |>
					dplyr::add_row(rd_est = rep(NA, 2), .before = 1)

				# Revise table names for this specific term
				names(rd) <- paste0(names(rd), '_', t)

				# Add to rates
				rates <- dplyr::bind_cols(rates, rd)
			}


			# Make compatible with binding
			unadj <-
				rates[-1, ] |>
				dplyr::mutate(label = ifelse(label == 'risk', lvl_nms[1], label)) |>
				tibble::add_row(label = as.character(adj$number[-c(1:2)]))

			# Bind the tables together
			# Order of columns is that reference is on left
			# Each level is then with incidence rate and to the right is hazard

			lvls <- levels(dat[[t]]) # Reference is first
			vars <- unique(grep(t, obj$term, value = TRUE))

			stopifnot(
				'Levels of variables are not consistent within the specificed term'
				= length(lvls)  == length(vars) + 1
			)

			# Basic table merge
			tbl <-
				dplyr::bind_cols(unadj, adj) |>
				dplyr::select(-number)

			# Organize columns
			for (i in seq_along(vars)) {
				tbl <-
					tbl |>
					dplyr::relocate(contains(vars[i]), .after = lvls[i + 1])
			}

			# Rename columns
			names(tbl)[names(tbl) %in% names(n)] <-
				paste0(names(n), ' (n = ', n, ')')

			# We are rotating through each outcome for a single term
			tbl_out[[o]] <- tbl

		}

		# Now the outcomes can be bound together
		# Don't want to duplicate the outcome and label lines so will drop if dups
		if (length(tbl_tms) == 0) {
			tbl_tms[[t]] <- dplyr::bind_rows(tbl_out)
		} else {
			tbl_tms[[t]] <- dplyr::bind_rows(tbl_out) |>
				dplyr::select(-label, -outcome)
		}

	}

	# Now the terms are bound together and clean up
	tab <-
		dplyr::bind_cols(tbl_tms) |>
		dplyr::mutate(label = as.character(factor(
			label,
			levels = c('events', lvl_nms),
			labels = c('Total No. of events', lvl_lab)
		))) |>
		dplyr::mutate(outcome = factor(outcome, levels = out_nms, labels = out_lab))

	# Convert into gt table
	gtbl <-
		gt(tab, rowname_col = 'label', groupname_col = 'outcome') |>
		cols_hide(columns = contains('p_value')) |>
		sub_missing(missing_text = '') |>
		fmt_number(drop_trailing_zeros = TRUE)


	# Make changes to table programmatically
	for (t in tms_nms) {

		# Specific variables for each term (which is essentially a column group)
		vars <- unique(grep(t, obj$term, value = TRUE))

		# Merge columns together
		for (v in vars) {
			gtbl <-
				gtbl |>
				cols_merge(
					columns = starts_with(v),
					pattern = '{1} ({2}, {3})',
					rows = contains(lvl_lab)
				)

			if (rate_difference & length(levels(dat[[t]])) == 2) {
				gtbl <-
					gtbl |>
					cols_merge(
						columns = starts_with('rd_') & ends_with(v),
						pattern = '{1} ({2}, {3})',
						rows = contains('Rate')
					)
			}
		}

		# Rename hazard ratio columns
		col_lab <- rep('HR (95% CI)', length(vars))
		names(col_lab) <- paste0(vars, '_estimate')
		gtbl <- gtbl |> cols_label(.list = col_lab)

		# Rename rate differences
		gtbl <-
			gtbl |>
			cols_label(contains('rd_') ~ 'Rate difference per 100 patient-years (95% CI)')

		# Add table spanners for terms
		#		Dichotomous
		# 		If there is only one variable (e.g. 2 levels), then spanner length = 2
		# 	Categorical
		# 		If there are more than 2 levels, then spanner should be longer
		# 		Should start at first term, and go over all the HR until the end

		cols <- gtbl[['_boxhead']]$var
		if (length(vars) == 1) {
			left <- min(grep(vars, cols))
			right <- max(grep(vars, cols))
			span <- (left - 2):(right - 1)
		} else if (length(vars) > 1) {
			left <-
				vars |>
				sapply(function(.x) {
					min(grep(.x, cols))
				}) |>
				min()

			right <-
				vars |>
				sapply(function(.x) {
					max(grep(.x, cols))
				}) |>
				max()

			span <- (left - 2):(right) # Spans left to cover reference group
		}

		gtbl <-
			gtbl |>
			tab_spanner(label = tms[[t]], columns = span)
	}



	# Visual modifications
	# 	Indent adjustment lines
	# 	Align headings
	gtbl <-
		gtbl |>
		tab_stub_indent(rows = contains(lvl_lab[-1]), indent = 3) |>
		opt_align_table_header('left') |>
		tab_style(
			style = cell_text(align = 'left'),
			locations = cells_column_spanners()
		) |>
		tab_style(
			style = cell_text(align = 'left'),
			locations = cells_column_labels()
		) |>
		tab_style(
			style = cell_text(align = 'left'),
			locations = cells_body()
		)

	# Return gt table
	gtbl
}

