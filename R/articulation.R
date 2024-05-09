# These is the core class, hidden, that makes a causal relationship
# Connects an outcome and exposure or parameter by an "articulation"
# Will use it to design the related concepts need to support a "joint" 

#' Create a new `articulation` object
#' 
#' @export
articulate <- function() {
  
  new_articulation()
}

#' @keywords internal
#' @noRd
new_articulation <- function() {
  
  # An articulation or joint here describes a relationship in context
  # Descriptive elements: 
  #   Variable type = categorical, binary, continuous
  # Relationship attributes include:
  #   Model estimates and statistics (e.g. estimates, CI, p-value)
  #   Mean values by group (if applicable based on description type)
  # Contexts include:
  #   Adjustment = sequential or parallel adjustment status
  #   Strata = stratifying variable, which in turn depends on level + dataset
  #   Datasets = number of observations included
  
  context <- list(
    adjustment_type = "pattern",
    adjustment_number = n, # Essentially number of other covariates
    strata_variable = "strata",
    strata_level = c("level A", "level B"),
    
  )
  
  new_rcrd(
    list(
      relationship = relationship,
      context = context,
    ),
    class = "articulation"
  )
}