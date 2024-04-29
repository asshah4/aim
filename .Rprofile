# Set up devtools
if (interactive()) {
	suppressMessages(require(devtools))
}

# Colors
if (interactive() && requireNamespace("rsthemes", quietly = TRUE)) {
	# Set preferred themes if not handled elsewhere..
	rsthemes::set_theme_light("Solarized Light")  # light theme
	rsthemes::set_theme_dark("Solarized Dark") # dark theme

	# Whenever the R session restarts inside RStudio...
	setHook("rstudio.sessionInit", function(isNewSession) {
		# Automatically choose the correct theme based on time of day
		rsthemes::use_theme_auto(dark_start = "20:00", dark_end = "8:00")
	}, action = "append")
}

