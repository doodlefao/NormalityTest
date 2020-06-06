#' Normality test for 1 Variable
#'
#' @param x: tested variable
#' @param output: if "none", nothing is displayed, if "figure", a figure is displayed, if "message" a message is displayed, and  if "all", a message and a figure are displayed. Try and see !
#' @param var.name: the name to be displayed. The default value is the varname of x. Try and see !
#'
#' @return Output
#'   * pval: p-value of the normality test
#'   * result: text containing the results from the normality test
#'
#' @noRd
#'
#' @import graphics stats
#'
#' @examples
#' # Example 1:
#'---------------------------------------------
#' apply test.normality to 1 variable
#'---------------------------------------------
#'test.normality(iris$Sepal.Length, var.name = "Sepal Length")
#'test.normality(iris$Sepal.Length, var.name = "Sepal Length", output = "figure")
#'
#' Example 2:
#'---------------------------------------------
#' apply test.normality to multiple variables
#'---------------------------------------------
#' for (i in 1:ncol(iris)) {
#'   if (is.numeric(iris[, i]) == TRUE) {
#'     test.normality(iris[, i], var.name = names(iris)[i], output = "message")
#'   } else {
#'     cat("The variable", names(iris)[i], "is not numerical\n")
#'   }
#' }
#'

test.normality <- function(x, output = c("none", "figure", "message", "all"), var.name = deparse(substitute(x))) {
  output <- match.arg(output)
  if (!is.numeric(x)) {
    text <- paste0("The variable ", var.name, " is not numerical")
    if (output %in% c("message", "all")) {
      cat(text, "\n \n")
    }
    return(list(pval = NA, result = text))
  }
  if (output %in% c("figure", "all")) {
    par(mfrow = c(1, 2))
    test <- dnorm(x, mean = mean(x), sd = sd(x))
    histo <- hist(x, plot = FALSE)
    limiteY <- max(test, histo$density)
    hist(x, prob = TRUE, col = "lightgrey", main = "", plot = TRUE, ylim = c(0, limiteY), xlab = "", ylab = "Density")
    curve(dnorm(x, mean = mean(x), sd = sd(x)), col = "blue", add = TRUE)
    qqnorm(x, main = "")
    qqline(x)
    legend("topleft", legend = paste(c("Mean:", "Standard deviation:"), round(c(mean(x), sd(x)), 2)))
    mtext(paste("Normality test for the variable", var.name), 3, outer = TRUE, line = -2, cex = 1.5)
  }
  pvalue <- shapiro.test(x)$p.value
  if (pvalue > .05) {
    text <- paste0("The variable ", var.name, " is normal according to the Shapiro Wilk test (p-value=", round(pvalue, 5), ")")
  } else {
    text <- paste0("The variable ", var.name, " is not normal according to the Shapiro Wilk test (p-value=", round(pvalue, 5), ")")
  }
  if (output %in% c("message", "all")) {
    cat(text, "\n \n")
  }
  return(list(pval = pvalue, result = text))
}

