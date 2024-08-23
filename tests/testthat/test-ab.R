test_that("antibody models work", {
  # Time of doses (assuming fist is dose 3 of primary series)
  td <- c(1, 365, 365 * 2)
  # Peaks following primary series or boost
  init_titres <- c(100, 80, 90)
  # Proportion short-lived
  prop_short <- c(1, 0.5, 0.6)
  # Decay short
  dur_short <- c(60, 60, 60)
  # Decay long
  dur_long <- c(300, 300, 300)
  t <- 5 * 365

  # Single dose
  abr <- ab_r(1:t, td[1], init_titres[1], prop_short[1], dur_short[1], dur_long[1])
  abc <- ab_cpp(1:t, td[1], init_titres[1], prop_short[1], dur_short[1], dur_long[1])
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Multiple doses
  abr <- ab_r(1:t, td, init_titres, prop_short, dur_short, dur_long)
  abc <- ab_cpp(1:t, td, init_titres, prop_short, dur_short, dur_long)
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Drop in titre
  init_titres[2] <- 1
  abr <- ab_r(1:t, td, init_titres, prop_short, dur_short, dur_long)
  abc <- ab_cpp(1:t, td, init_titres, prop_short, dur_short, dur_long)
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))
})


test_that("antibody model wrapper works", {
  # Time of doses (assuming fist is dose 3 of primary series)
  td <- c(1, 365, 365 * 2)
  # Peaks following primary series or boost
  init_titres <- c(100, 80, 90)
  # Proportion short-lived
  prop_short <- c(1, 0.5, 0.6)
  # Decay short
  dur_short <- c(60, 60, 60)
  # Decay long
  dur_long <- c(300, 300, 300)
  t <- 5 * 365

  # Single dose
  abr <- ab(t, td[1], init_titres[1], prop_short[1], dur_short[1], dur_long[1], cpp = FALSE)
  abc <- ab(t, td[1], init_titres[1], prop_short[1], dur_short[1], dur_long[1])
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Multiple doses
  abr <- ab(t, td, init_titres, prop_short, dur_short, dur_long, cpp = FALSE)
  abc <- ab(t, td, init_titres, prop_short, dur_short, dur_long)
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Drop in titre
  init_titres[2] <- 1
  abr <- ab(t, td, init_titres, prop_short, dur_short, dur_long, cpp = FALSE)
  abc <- ab(t, td, init_titres, prop_short, dur_short, dur_long)
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Errors correctly
  expect_error(
    ab(-1, td, init_titres, prop_short, dur_short, dur_long),
    "timesteps must be positive"
  )
  td_wrong <- td
  td_wrong[2] <- -1
  expect_error(
    ab(t, td_wrong, init_titres, prop_short, dur_short, dur_long),
    "dose_timesteps must be positive and montonically increasing"
  )
  td_wrong <- td
  td_wrong[2] <- 1000
  expect_error(
    ab(t, td_wrong, init_titres, prop_short, dur_short, dur_long),
    "dose_timesteps must be positive and montonically increasing"
  )
  init_wrong <- init_titres
  init_wrong[2] <- -1
  expect_error(
    ab(t, td, init_wrong, prop_short, dur_short, dur_long),
    "All init_titres must be > 0"
  )
  prop_wrong <- prop_short
  prop_wrong[2] <- -1
  expect_error(
    ab(t, td, init_titres, prop_wrong, dur_short, dur_long),
    "prop_short must be between 0 and 1"
  )
  prop_wrong[2] <- 2
  expect_error(
    ab(t, td, init_titres, prop_wrong, dur_short, dur_long),
    "prop_short must be between 0 and 1"
  )
  dur_wrong <- dur_short
  dur_wrong[2] <- 1000
  expect_error(
    ab(t, td, init_titres, prop_short, dur_wrong, dur_long),
    "dur_short should be < dur_long"
  )
})
