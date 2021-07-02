# Reporting as percent in tables

#' Print a numeric as a nice percent
#' @param x a numeric between 0 and 1
#' @param dig number of digits to print
#' @return
#' @export
as.percent <- function(x, dig = 1) {
  paste0(format(round(100*(x), dig), nsmall = dig), "%")
}

# usa_sum <- function(x) sum(x*usa_dem)/sum(usa_dem)

#' Print a confidence interval
#' @param x sample
#' @param ci width of the interval
#' @param dig number of digits to print
#' @return
#' @importFrom stats quantile
#' @export
ci <- function(x, ci = .95, dig = 2) {
  paste3(mean(x), quantile(x, (1-ci)/2), quantile(x, 1 - (1-ci)/2), dig)
}

#' Paste 2 values into mean (SD) format
#' @param x mean
#' @param y SD
#' @param d number of digits to display
#' @return
#' @export
paste2 <- function(x,y, d=2){
  paste0(format(x, digits = d), "(", format(y, digits = d), ")",
         sep = "")
}

#' Paste 3 values into mean (LCI, UCI) format
#' @param x mean
#' @param y LCI
#' @param z UCI
#' @param dig number of digits to display
#' @return
#' @export
paste3 <- function(x, y, z, dig = 2) {
  paste0(format(x, digits = dig, big.mark = ","), " (",
         format(y, digits = dig, big.mark = ","), ", ",
         format(z, digits = dig, big.mark = ","), ")",
         sep = "")
}

#' Divide by 100,000 (!)
#' @param x numeric
#' @return
#' @export
per100k <- function(x) {x/1e05}

#' Remove line breaks from character strings
#' @param x string
#' @return
#' @export
remove_par <- function(x) { gsub("\\(|\\)", "", x) }

#' Remove whitespaces and commas
#' @param x string
#' @return
#' @export
asnum <- function(x) {
  x <- gsub(",", "", x)
  x <- gsub(" ", "", x)
  x <- as.numeric(x)
}

#' Logit
#' @param x numeric
#' @return
#' @export
logit <- function(x) log(x/(1-x))

#' Inverse logit
#' @param x numeric
#' @return
#' @export
inv_logit <- function(x) {exp(x)/(1+exp(x))}


#' Check if a vector is monotonic
#'
#' @param x numerical vector
#' @param type matched to `"growing"` or `"decreasing"`
#' @return TRUE/FALSE
#' @export
is.monotonic <- function(x, type = "growing") {
  if(!is.numeric(x))
    stop("Checking monotonicity of a non-numeric vector")
  if(length(x) == 1)
    return(TRUE)
  type <- match.arg(type, choices = c("growing", "decreasing"))
  if(type == "growing")
    for(i in 2:length(x))
      if(x[i] <= x[i-1])
        return(FALSE)
  if(type == "decreasing")
    for(i in 2:length(x))
      if(x[i] >= x[i-1])
        return(FALSE)
  return(TRUE)
}


#' Return proportion of the observations that are less or greater than some value
#' @return A numeric between 0 and 1
#' @export
gt <- function(x, y) {
  sum(x > y)/length(x)
}
#' @export
#' @describeIn gt Proportion of `x` greater or equal to `y`
gte <- function(x, y) {
  sum(x >= y)/length(x)
}
#' @export
#' @describeIn gt Proportion of `x` < `y`
lt <- function(x, y) {
  sum(x < y)/length(x)
}
#' @export
#' @describeIn gt Proportion of `x` <= `y`
lte <- function(x, y) {
  sum(x <= y)/length(x)
}

#' @export
#' @describeIn gt Proportion of `x` > 0
gt0 <- function(x) {
  gt(x, 0)
}
#' @export
#' @describeIn gt Proportion of `x` >= 0
gte0 <- function(x) {
  gte(x, 0)
}
#' @export
#' @describeIn gt Proportion of `x` < 0
lt0 <- function(x) {
  lt(x, 0)
}
#' @export
#' @describeIn gt Proportion of `x` <= 0
lte0 <- function(x) {
  lte(x, 0)
}


#' Return .Last.value
#' @export
lv <- function() {
  .Last.value
}


#' Standardise a vector
#'
#' @param x vector to standardise (x - mean)/sd
#' @param skip_binom if there are only 2 unique values, should they be skipped?
#' @return a standardised vector
#'
st <- function(x, skip_binom = FALSE){
  if(skip_binom && (length(unique(x)) == 2)) return(x)
  (x - mean(x))/sd(x)
}
