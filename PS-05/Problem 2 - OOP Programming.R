setClass("poly",
         slots = c(
           coefficients = "numeric",
           powers = "integer"))

make_poly <- function(coefficients, powers){
  return(new("poly", coefficients = coefficients, powers = powers))
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/duplicated.html#:~:text=duplicated()%20determines%20which%20elements,elements%20(rows)%20are%20duplicates.
setValidity("poly", function(object){
  if (length(object@coefficient) != length(object@powers)){
    stop("Lengths of coefficients and powers must be equal")
  }
  if (any(object@powers < 0)){
    stop("Powers must be non-negative integer")
  }
  if (any(duplicated(object@powers))) {
    stop("Powers cannot be duplicated")
  }
  return(TRUE)
})
