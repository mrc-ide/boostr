test_that("antibody models work", {
  titre <- as.numeric(1:100)
  max_efficacy <- 0.9
  alpha <- 0.6
  beta <- 100.0

  # Single efficacy
  er <- efficacy_r(titre[1], max_efficacy, alpha, beta)
  ec <- efficacy_cpp(titre[1], max_efficacy, alpha, beta)
  expect_identical(er, ec)
  expect_true(all(er > 0))
  expect_true(all(ec > 0))

  er <- efficacy_r(titre, max_efficacy, alpha, beta)
  ec <- efficacy_cpp(titre, max_efficacy, alpha, beta)
  expect_identical(er, ec)
  expect_true(all(er > 0))
  expect_true(all(ec > 0))
})

test_that("antibody model wrapper works", {
  titre <- as.numeric(1:100)
  max_efficacy <- 0.9
  alpha <- 0.6
  beta <- 100.0

  # Single efficacy
  er <- efficacy(titre[1], max_efficacy, alpha, beta, cpp = FALSE)
  ec <- efficacy(titre[1], max_efficacy, alpha, beta)
  expect_identical(er, ec)
  expect_true(all(er > 0))
  expect_true(all(ec > 0))

  er <- efficacy(titre, max_efficacy, alpha, beta, cpp = FALSE)
  ec <- efficacy(titre, max_efficacy, alpha, beta)
  expect_identical(er, ec)
  expect_true(all(er > 0))
  expect_true(all(ec > 0))

  titre_wrong <- titre
  titre_wrong[2] <- -1
  expect_error(
    efficacy(titre_wrong, max_efficacy, alpha, beta),
    "All titres must be > 0"
  )
  max_ef_wrong <- -1
  expect_error(
    efficacy(titre, max_ef_wrong, alpha, beta),
    "max_efficacy must be between 0 and 1"
  )
  max_ef_wrong <- 2
  expect_error(
    efficacy(titre, max_ef_wrong, alpha, beta),
    "max_efficacy must be between 0 and 1"
  )
  alpha_wrong <- -1
  expect_error(
    efficacy(titre, max_efficacy, alpha_wrong, beta),
    "alpha must be > 0"
  )
  beta_wrong <- -1
  expect_error(
    efficacy(titre, max_efficacy, alpha, beta_wrong),
    "beta must be > 0"
  )
})
