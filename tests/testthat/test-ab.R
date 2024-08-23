test_that("antibody models work", {
  # Time of doses (assuming fist is dose 3 of primary series)
  td <- c(1, 365, 365 * 2)
  # Peaks following primary series or boost
  cs <- c(100, 80, 90)
  # Proportion short-lived
  rho <- c(1, 0.5, 0.6)
  # Decay short
  ds <- c(30, 30, 30)
  # Decay long
  dl <- c(300, 300, 300)
  t <- 5 * 365

  # Single dose
  abr <- ab_r(1:t, td[1], cs[1], rho[1], ds[1], dl[1])
  abc <- ab_cpp(1:t, td[1], cs[1], rho[1], ds[1], dl[1])
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Multiple doses
  abr <- ab_r(1:t, td, cs, rho, ds, dl)
  abc <- ab_cpp(1:t, td, cs, rho, ds, dl)
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Drop in titre
  cs[2] <- 1
  abr <- ab_r(1:t, td, cs, rho, ds, dl)
  abc <- ab_cpp(1:t, td, cs, rho, ds, dl)
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))
})


test_that("antibody model wrapper works", {
  # Time of doses (assuming fist is dose 3 of primary series)
  td <- c(1, 365, 365 * 2)
  # Peaks following primary series or boost
  cs <- c(100, 80, 90)
  # Proportion short-lived
  rho <- c(1, 0.5, 0.6)
  # Decay short
  ds <- c(30, 30, 30)
  # Decay long
  dl <- c(300, 300, 300)
  t <- 5 * 365

  # Single dose
  abr <- ab(t, td[1], cs[1], rho[1], ds[1], dl[1], cpp = FALSE)
  abc <- ab(t, td[1], cs[1], rho[1], ds[1], dl[1])
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Multiple doses
  abr <- ab(t, td, cs, rho, ds, dl, cpp = FALSE)
  abc <- ab(t, td, cs, rho, ds, dl)
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Drop in titre
  cs[2] <- 1
  abr <- ab(t, td, cs, rho, ds, dl, cpp = FALSE)
  abc <- ab(t, td, cs, rho, ds, dl)
  expect_identical(abc, abc)
  expect_true(all(abr > 0))
  expect_true(all(abc > 0))

  # Errors correctly
  expect_error(
    ab(-1, td, cs, rho, ds, dl),
    "timesteps must be positive"
  )
  td_wrong <- td
  td_wrong[2] <- -1
  expect_error(
    ab(t, td_wrong, cs, rho, ds, dl),
    "dose_timesteps must be positive and montonically increasing"
  )
  td_wrong <- td
  td_wrong[2] <- 1000
  expect_error(
    ab(t, td_wrong, cs, rho, ds, dl),
    "dose_timesteps must be positive and montonically increasing"
  )
  init_wrong <- cs
  init_wrong[2] <- -1
  expect_error(
    ab(t, td, init_wrong, rho, ds, dl),
    "All init_titres must be > 0"
  )
  prop_wrong <- rho
  prop_wrong[2] <- -1
  expect_error(
    ab(t, td, cs, prop_wrong, ds, dl),
    "prop_short must be between 0 and 1"
  )
  prop_wrong[2] <- 2
  expect_error(
    ab(t, td, cs, prop_wrong, ds, dl),
    "prop_short must be between 0 and 1"
  )
  dur_wrong <- ds
  dur_wrong[2] <- 1000
  expect_error(
    ab(t, td, cs, rho, dur_wrong, dl),
    "dur_short should be < dur_long"
  )
})
