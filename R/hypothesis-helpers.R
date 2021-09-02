#' Update a Hypothesis
#'
#' A `hypothesis` object can be modified to generate an updated hypothesis,
#' which can then be placed into a `framework` as needed. The **strata** and
#' **test_opts** arguments can only be updated if the **data** and **test**
#' options are also given, respectively.
#'
#' @return A `hypothesis` object
#'
#' @param hypothesis A `hypothesis` object
#' @param ... Additional named arguments to pass (should be named components of
#'   a `hypothesis` object)
#'
#' @export
update_hypothesis <- function(hypothesis, ...) {

	mc <- match.call(expand.dots = TRUE)

	changes <- list(...)

	new_names <- names(changes)
	old_names <- names(attributes(hypothesis))

	for (i in new_names) {
		# Ensure appropriate arguments
		if (!(i %in% old_names)) {
			stop("The argument `",
					 i,
					 "` is not a valid argument for a `hypothesis` object")
		}

		# Update combination
		if (i == "combination") {
			attributes(hypothesis)$combination <- changes[[i]]
		}
		# Old data should be removed before placing new data
		else if (i == "data") {
			attributes(hypothesis)$data <- changes[[i]]
			new_data_name <- mc[[i]]
			attributes(hypothesis)$data_name <- as.character(new_data_name)
		}
		# Modify formula if strata is new
		else if (i == "strata") {
			a <- attributes(hypothesis)
			hypothesis <-
				stats::update(hypothesis, bquote(. ~ . - .(as.name(changes[[i]]))))
			attributes(hypothesis) <- a
			attributes(hypothesis)$strata <- changes[[i]]
		}
		# Replace the old with the new
		else {
			attributes(hypothesis)[[i]] <- changes[[i]]
		}
	}

	invisible(hypothesis)

}
