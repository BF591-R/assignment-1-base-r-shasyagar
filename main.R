# ----------------------- Helper Functions to Implement ------------------------

#' Evaluate whether the argument is less than 0
#'
#' Returns TRUE if the numeric argument x is less than 0, otherwise returns FALSE
#'
#' @param x (numeric): the numeric value(s) to test
#'
#' @return logical value or vector indicating whether the numeric argument is less than 0
#' @export
#'
#' @examples
#' less_than_zero(-1)
#' [1] TRUE
#' less_than_zero(10)
#' [1] FALSE
#' less_than_zero(c(-1,0,1,2,3,4))
#' [1] TRUE FALSE FALSE FALSE FALSE FALSE
less_than_zero <- function(x){
  return(x < 0)  # TRUE for values less than 0, else FALSE
}


#' Evaluate whether the argument is between two numbers
#'
#' Returns TRUE if the numeric argument x is contained within the open interval
#' (a, b), otherwise return FALSE.
#'
#' @param x (numeric): the numeric value(s) to test
#' @param a (number): the lower bound
#' @param b (number): the upper bound
#'
#' @return logical value of same type as input (e.g. scalar, vector, or matrix)
#' @export
#'
#' @examples
#' is_between(3,1,5)
#' [1] TRUE
#' is_between(c(1,9,5,2), 1, 5)
#' [1] FALSE FALSE FALSE TRUE
#' is_between(matrix(1:9, nrow=3, byrow=TRUE), 1, 5)
#'       [,1]  [,2]  [,3]
#' [1,] FALSE  TRUE  TRUE
#' [2,]  TRUE FALSE FALSE
#' [3,] FALSE FALSE FALSE
is_between <- function(x, a, b) {
  return(x > a & x < b)  # TRUE if x is between a & b
}


#' Return the values of the input vector that are not NA
#'
#' Returns the values of the input vector `x` that are not NA
#'
#' @param x (numeric): numeric vector to remove NA from 
#'
#' @return numeric vector `x` with all `NA` removed
#' @export
#'
#' @examples
#' x <- c(1,2,NA,3)
#' rm_na(x)
#' [1] 1 2 3
rm_na <- function(x) {
  return(as.vector(na.omit(x)))  # remove NAs, and as.vector() to remove attributes
}
x <- c(1,2,NA,3)
rm_na(x)

#' Calculate the median of each row of a matrix
#'
#' Given the matrix x with n rows and m columns, return a numeric vector of
#' length n that contains the median value of each row of x
#'
#' @param x (numeric matrix): matrix to compute median along rows
#'
#' @return (numeric vector) vector containing median row values
#' @export
#'
#' @examples
#' m <- matrix(1:9, nrow=3, byrow=T)
#' row_medians(m)
#' [1] 2 5 8
row_medians <- function(x) {
  return(apply(x, 1, median, na.rm = TRUE))  # median function to each row
}


#' Evaluate each row of a matrix with a provided function
#'
#' Given the matrix `x` with n rows and m columns, return a numeric vector of
#' length n that contains the returned values from the evaluation of the
#' function `fn` on each row. `fn` should be a function that accepts a vector as
#' input and returns a scalar value
#'
#' @param x (numeric matrix): matrix to evaluate the function along rows
#' @param fn (function) function that accepts a vector as input and returns a scalar
#' @param na.rm (logical) OPTIONAL: a logical evaluating to `TRUE` or `FALSE`
#'   indicating whether `NA` values should be stripped before computing summary
#'
#' @return (numeric vector) vector of the evaluation of `fn` on rows of `x`
#' @export
#'
#' @examples
#' m <- matrix(1:9, nrow=3, byrow=T)
#' summarize_rows(m, min)
#' [1] 1 4 7
#' summarize_rows(m, mean)
#' [1] 2 5 8
summarize_rows <- function(x, fn, na.rm=FALSE) {
  return(apply(x, 1, fn, na.rm = na.rm))  # function `fn` to each row
}


#' Summarize matrix rows into data frame
#'
#' Summarizes the rows of matrix `x`. Returns a data frame with the following
#' columns in order and the corresponding value for each row:
#' 
#'   * mean - arithmetic mean
#'   * stdev - standard deviation
#'   * median - median
#'   * min - minimum value
#'   * max - maximum value
#'   * num_lt_0 - the number of values less than 0
#'   * num_btw_1_and_5 - the number of values between 1 and 5
#'   * num_na - the number of missing (NA) values (ignoring value of na.rm)
#'
#' @param x (numeric matrix): matrix to evaluate the function along rows
#' @param na.rm (logical) OPTIONAL: a logical evaluating to `TRUE` or `FALSE`
#'   indicating whether `NA` values should be stripped before computing summary
#'
#' @return (data frame) data frame containing summarized values for each row of `x`
#' @export
#'
#' @examples
# m <- matrix(1:9, nrow=3, byrow=T)
# summarize_matrix(m)
#   mean stdev median min max num_lt_0 num_btw_1_and_5 num_na
#' 1    2     1      2   1   3        0               3      0
#' 2    5     1      5   4   6        0               1      0
#' 3    8     1      8   7   9        0               0      0
summarize_matrix <- function(x, na.rm=FALSE) {
  mean_vals <- apply(x, 1, mean, na.rm = na.rm)
  stdev_vals <- apply(x, 1, sd, na.rm = na.rm)
  median_vals <- apply(x, 1, median, na.rm = na.rm)
  min_vals <- apply(x, 1, min, na.rm = na.rm)
  max_vals <- apply(x, 1, max, na.rm = na.rm)
  num_lt_0 <- apply(x, 1, function(row) sum(row < 0, na.rm = na.rm))
  num_btw_1_and_5 <- apply(x, 1, function(row) sum(row > 1 & row < 5, na.rm = na.rm))
  num_na <- apply(x, 1, function(row) sum(is.na(row)))
  
  # Return as a data frame
  return(data.frame(
    mean = mean_vals,
    stdev = stdev_vals,
    median = median_vals,
    min = min_vals,
    max = max_vals,
    num_lt_0 = num_lt_0,
    num_btw_1_and_5 = num_btw_1_and_5,
    num_na = num_na
  ))
}


# ------------ Helper Functions Used By Assignment, You May Ignore ------------

sample_normal <- function(n, mean=0, sd=1) {
  return(rnorm(n, mean, sd))  # Generate a vector of random normal values
}

sample_normal_w_missing <- function(n, mean=0, sd=1, missing_frac=0.1) {
  vals <- rnorm(n, mean, sd)
  missing_idx <- sample(1:n, size = floor(missing_frac * n), replace = FALSE)
  vals[missing_idx] <- NA
  return(vals)
}

simulate_gene_expression <- function(num_samples, num_genes) {
  return(matrix(rnorm(num_samples * num_genes), nrow=num_samples))
}

simulate_gene_expression_w_missing <- function(num_samples, num_genes, missing_frac=0.1) {
  mat <- matrix(rnorm(num_samples * num_genes), nrow=num_samples)
  missing_idx <- sample(1:(num_samples * num_genes), size = floor(missing_frac * num_samples * num_genes), replace = FALSE)
  mat[missing_idx] <- NA
  return(mat)
}
