setClass("poly",
         slots = c(
           coefficients = "numeric",
           powers = "numeric"))

make_poly <- function(coefficients, powers){
  return(new("poly", coefficients = coefficients, powers = powers))
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/duplicated.html#:~:text=duplicated()%20determines%20which%20elements,elements%20(rows)%20are%20duplicates.
setValidity("poly", function(object){
  if (length(object@coefficients) != length(object@powers)){
    stop("Lengths of coefficients and powers must be equal")
  }
  if (any(object@powers < 0)){
    stop("Powers must be non-negative integer")
  }
  if (any(duplicated(object@powers))) {
    stop("Powers cannot be duplicated")
  }
  if (any(object@powers != as.integer(object@powers))) {
    stop("Powers must be integers")
  }
  return(TRUE)
})

setMethod("show", "poly", function(object) {
  non_zero_indices <- which(object@coefficients != 0)
  non_zero_coefficients <- object@coefficients[non_zero_indices]
  non_zero_powers <- object@powers[non_zero_indices]
  terms <- paste0(non_zero_coefficients, "x^", non_zero_powers)
  terms <- sub("x\\^0", "", terms)
  terms <- sub("1x", "x", terms)
  terms <- paste(terms, collapse = " + ")
  cat(gsub("\\+ -", "- ", terms))
})

setMethod("+", c("poly", "poly"), function(e1, e2) {
  e1_list <- setNames(e1@coefficients,as.character(e1@powers))
  e2_list <- setNames(e2@coefficients,as.character(e2@powers))
  all_powers <- as.character(sort(unique(c(e1@powers, e2@powers)), decreasing = TRUE))
  combined_list <- setNames(numeric(length(all_powers)), as.character(all_powers))
  for (power in all_powers) {
    combined_list[power] <- sum(e1_list[power], e2_list[power], na.rm = TRUE)
  }
  return(make_poly(combined_list, as.integer(names(combined_list))))
})
  
setMethod("-", c("poly", "poly"), function(e1, e2) {
  e2@coefficients <- -e2@coefficients
  return(e1 + e2)
})

p1 <- make_poly(c(3, 2), c(2, 0))
p2 <- make_poly(c(7, -2, -1, 17), c(3, 2, 1, 0))

p1
p2

p1 + p2
p1 - p2