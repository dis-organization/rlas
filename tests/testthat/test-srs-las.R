context("srs-las")

## TODO: Rename context
## TODO: Add more tests

test_that("basic read works", {
  expect_that(s <- lasfile(system.file("extdata/lasfiles/srs.las", package = "rlas")), gives_warning("is only valid"))
  expect_that(s, is_a("data.frame"))
  expect_that(names(s), equals(c("x", "y", "z", "gpstime", "intensity")))
  expect_that(unlist(lapply(s, class)), equals(structure(c("numeric", "numeric", "numeric", "numeric", "numeric"
  ), .Names = c("x", "y", "z", "gpstime", "intensity"))))
})
