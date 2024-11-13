test_that("Efficacy model work", {
  titre <- as.numeric(1:100)
  max_efficacy <- 0.9
  alpha <- 0.6
  beta <- 100.0

  # Single efficacy
  er <- efficacy(titre[1], max_efficacy, alpha, beta)
  expect_true(all(er > 0))

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
