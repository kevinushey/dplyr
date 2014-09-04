##' Make a Data Frame
##'
##' A trimmed down version of \code{\link{data.frame}} that:
##'
##' 1. Evaluates its arguments lazily and in order,
##' 2. Foregoes the option of providing \code{row.names},
##' 3. Foregoes column name checking (except that we confirm names are not empty),
##' 4. Does not convert strings to factors,
##' 5. Performs no conersion of inputs. Strings remain as strings, lists as
##'   lists, and so on.
##'
##' @param ... A set of (named) arguments. Unnamed arguments will be inferred
##'   by deparsing the expression used in their construction.
##' @examples
##' # Note how arguments are looked up preferentially, and in order,
##' # within the data.frame being constructed
##' a <- 1
##' data_frame(x = a, y = x * 2)
##' data_frame(a = 5, y = a) ## y is 5, not 1!
##' @rdname data-frame.R
##' @export
data_frame <- function(...) {
  data_frame_(lazy::lazy_dots(...))
}

##' @rdname data-frame.R
##' @param dots A \code{lazy::lazy_dots()} object, as returned
##' @export
data_frame_ <- function(dots) {

  ## Get the dots used
  dots_nm <- names(dots)
  n <- length(dots)

  ## Early escape for dots with no arguments
  if (n == 0) return(data.frame())

  ## Construct the list output
  output <- vector("list", n)
  names(output) <- character(n)

  ## Fill the output
  fill_lazy(output, dots, dots_nm)

  ## Recycle (note: in place; returns success as logical)
  ok <- recycle(output)
  if (!ok) {
    lengths <- vapply(output, NROW, integer(1))
    if (length(unique(lengths)) > 1) {
      stop("arguments imply differing number of rows: ",
           paste(lengths, collapse = ", "), call. = TRUE)
    }
  }

  ## Set attributes
  n <- length(output[[1]])
  setattr(output, "row.names", .set_row_names(n))
  setattr(output, "class", c("tbl_df", "tbl", "data.frame"))

  output
}
