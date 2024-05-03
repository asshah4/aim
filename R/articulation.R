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
  
  new_rcrd(
    list(
      "left" = left,
      "right" = right,
      "model" = model
    ),
    class = "articulation"
  )
}