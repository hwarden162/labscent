#' Calculate The Homogeneity of Cell Gene Expression in scRNA-seq Data.
#'
#' Use entropy methods from Michael J. Casey to quantify homogeneity using
#' information theory.
#'
#' @param counts The counts matrix. Rows must be cells and columns must be genes.
#' @param normalise Whether the homogeneity should be returned as a proportion of
#'     the maximum possible homogeneity.
#' @param ... Arguments passed to `entropy::entropy()`. It is recommended these are
#'     not included when using `normalise = TRUE`.
#'
#' @return A vector of homogeneity scores.
#' @export
#'
#' @examples
#'
#' mat <- matrix(1:12, ncol = 3)
#' calc_hom(mat, normalise = TRUE)
#'
calc_hom <- function(counts = NULL, normalise = FALSE, ...) {
  if (is.null(counts)) {
    stop("\n \u2716 No counts matrix has been specified")
  }

  norm <- apply(
    counts,
    1,
    \(x) {x/sum(x)}
  )
  norm <- t(norm)

  hom <- apply(
    norm,
    1,
    \(x) {entropy::entropy(x)}
  )

  if (normalise) {
    hom <- hom/log(length(hom))
  }

  hom
}
