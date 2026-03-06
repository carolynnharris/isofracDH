#' Add single-component D/H fractionation (Sessions 2005, Section 3.1)
#'
#' Calculates a product isotope composition column based on a single source
#' fractionated by fractionation factor (alpha), following Sessions (2005) Section 3.1.
#'
#' @details
#' **Units:** Input `source_d2H` must be in per mille (‰). Internally, this
#' function converts \eqn{\delta} to fractional form (\eqn{\delta/1000}) when
#' applying the equation, then returns the result in per mille (‰).
#'
#' **Alpha:** `alpha` can be a single numeric value (used for all rows) or
#' a column in `df`.
#'
#' @param df A data.frame or tibble.
#' @param source_d2H Column containing source \eqn{\delta D} values in per mille (‰).
#' @param alpha Fractionation factor (\eqn{\alpha}). Either a single numeric value
#'   or a column in `df`.
#' @param name Name of the new output column to create. Default `"product_d2H"`.
#'
#' @return `df` with an added column containing the calculated product \eqn{\delta D}
#'   in per mille (‰).
#'
#' @examples
#' library(dplyr)
#'
#' df <- tibble::tibble(source_d2H = seq(-300, 50, by = 50))
#'
#' # alpha as a scalar
#' df |> add_frac_single(source_d2H = source_d2H, alpha = 0.92)
#'
#' # alpha as a column
#' df2 <- df |> mutate(alpha = seq(0.90, 0.94, length.out = n()))
#' df2 |> add_frac_single(source_d2H = source_d2H, alpha = alpha, name = "delta_prod")
#'
#' @export
add_frac_single <- function(df, source_d2H, alpha, name = "product_d2H") {
  if (!is.data.frame(df)) {
    rlang::abort("`df` must be a data.frame or tibble.")
  }
  if (!is.character(name) || length(name) != 1 || is.na(name) || nchar(name) == 0) {
    rlang::abort("`name` must be a single, non-empty character string.")
  }

  source_d2H_q <- rlang::enquo(source_d2H)
  alpha_q <- rlang::enquo(alpha)
  out_nm <- rlang::sym(name)

  df |>
    dplyr::mutate(
      # Convert permil to fractional form internally
      .source_d2H_frac = (!!source_d2H_q) / 1000,
      # alpha can be scalar or column; dplyr will recycle scalars
      .alpha_val = (!!alpha_q),
      # Sessions (2005) Section 3.1 form:
      # product_d2H = alpha * source_d2H + (alpha - 1), in fractional delta units
      .product_d2H_frac = (.alpha_val * .source_d2H_frac) + (.alpha_val - 1),
      # Convert back to permil
      !!out_nm := .product_d2H_frac * 1000
    ) |>
    dplyr::select(-.source_d2H_frac, -.alpha_val, -.product_d2H_frac)
}
