test_that("add_frac_single works with scalar alpha", {
  df <- tibble::tibble(source_d2H = c(-200, -100, 0, 50))
  out <- add_frac_single(df, source_d2H = source_d2H, alpha = 0.92)

  expect_true("product_d2H" %in% names(out))

  # Hand calculation (using the same internal delta/1000 convention)
  d2H_source_frac <- df$source_d2H / 1000
  alpha <- 0.92
  d2H_product_frac <- alpha * d2H_source_frac + (alpha - 1)
  expected <- d2H_product_frac * 1000

  expect_equal(out$product_d2H, expected, tolerance = 1e-12)
})

test_that("add_frac_single works with alpha as a column", {
  df <- tibble::tibble(
    source_d2H = c(-200, -100, 0, 50),
    alpha = c(0.90, 0.91, 0.92, 0.93)
  )
  out <- add_frac_single(df, source_d2H = source_d2H, alpha = alpha)

  d2H_source_frac <- df$source_d2H / 1000
  d2H_product_frac <- df$alpha * d2H_source_frac + (df$alpha - 1)
  expected <- d2H_product_frac * 1000

  expect_equal(out$product_d2H, expected, tolerance = 1e-12)
})

test_that("add_frac_single respects custom output name", {
  df <- tibble::tibble(source_d2H = c(-200, 0))
  out <- add_frac_single(df, source_d2H = source_d2H, alpha = 0.92, name = "dD_product")

  expect_true("dD_product" %in% names(out))
  expect_false("product_d2H" %in% names(out))
})

test_that("add_frac_single errors on bad inputs", {
  expect_error(add_frac_single("not_df", source_d2H = x, alpha = 0.9))
  expect_error(add_frac_single(tibble::tibble(x = 1), source_d2H = x, alpha = 0.9, name = ""))
})
