context("lmeasure")

test_that("convert 2 lm", {
  skip("not yet")
 f <- paste0(system.file("extdata", "one", package = "neurostrplus"), '/')
 b <- get_full_db(f)
 a <- convert2lm(b)
 expect_true(max(a$remote_bifurcation_angle, na.rm = TRUE) > 90)
})

test_that("height", {
  skip("not yet")
 f <- paste0(system.file("extdata", "one", package = "neurostrplus"), '/')
 b <- get_full_db(f)
 rw <- read.csv('../bbp-data/lmeasure-axon.csv', row.names = 1)
 rw <- rw[unique(b$neuron), ]
 reduced_diff <- unname(diff(quantile(b$x, probs = c(0.025, 0.975))))
 # Does not equal when removing 5% extreme
 # expect_equal(reduced_diff, rw[, c('Width_sum')])

 # Correlated
 f <- paste0(system.file("extdata", "ten", package = "neurostrplus"), '/')
 b <- get_full_db(f)
 vars <- compute_vars_both(b)
 rw <- read.csv('../bbp-data/lmeasure-axon.csv', row.names = 1)
 rw <- rw[unique(b$neuron), ]
 cors <- mapply(cor, rw[, c('Width_sum', 'Height_sum', 'Depth_sum')], vars[, 1:3])
 expect_true(all(cors > 0.9))
})

test_that("compute difference", {
 skip("not yet")
 f <- paste0(system.file("extdata", "one", package = "neurostrplus"), '/')
 b <- get_full_db(f)
 rw <- read.csv('../bbp-data/lmeasure-axon.csv', row.names = 1)
 rw <- rw[unique(b$neuron), ]
 vars <- compute_vars_both(b)
 diff <- compute_lm_difference(rw, vars)
 expect_true(diff$Partition_asymmetry_min == 0)
})
