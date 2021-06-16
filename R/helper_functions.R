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
