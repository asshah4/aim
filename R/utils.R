#' @description Check on the **which_arms** parameter
#' @noRd
bear_arms <- function(octomod, which_arms) {

	# Check which_arms argument
	if (is.null(which_arms)) {
		bear <- names(octomod$arms)
	} else {
		if (setequal(intersect(which_arms, names(octomod$core)), which_arms)) {
			stop("The named arms are not available in the `octomod`", call. = FALSE)
		} else {
			bear <- which_arms
		}
	}

	# Return character vector of arms
	bear

}

#' @description Validate the octomod structure within other functions
#' @noRd
validate_octomod <- function(octomod) {

	# Return
	TRUE
}
