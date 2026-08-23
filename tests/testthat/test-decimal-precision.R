test_that("decimal_places counts the original's precision", {
  expect_equal(decimal_places(c(27.7, 31.8, 31.5, 27.3)), 1L)
  expect_equal(decimal_places(c(1.25, 3.5, 10)), 2L)
  expect_equal(decimal_places(c(1, 2, 3)), 0L)
  expect_equal(decimal_places(c(NA, NaN, Inf)), 0L)
})

test_that("match_decimal_precision rounds synthetic to the original's decimals", {
  original  <- data.frame(x = c(27.7, 31.8, 31.5, 27.3), n = 1:4)
  synthetic <- data.frame(x = c(23.56576648668623, 27.99, 22.2953, 25.12), n = c(2.4, 3.6, 1.1, 4.9))

  out <- match_decimal_precision(synthetic, original)

  # x: original has 1 decimal -> synthetic rounded to 1 decimal
  expect_equal(out$x, c(23.6, 28.0, 22.3, 25.1))
  # n: original is integer-valued -> synthetic rounded to whole numbers
  expect_equal(out$n, c(2, 4, 1, 5))
})

test_that("synthesize_data output matches original decimal granularity", {
  set.seed(1)
  original <- data.frame(
    height = round(rnorm(60, 170, 8), 1),   # 1 decimal
    count  = sample(1:50, 60, replace = TRUE) # integer
  )
  spec <- synth_spec(purpose = "development", seed = 1)
  syn  <- synthesize_data(original, spec)

  dec_of <- function(v) max(decimal_places(v), 0L)
  expect_lte(dec_of(syn$height), 1L)
  expect_equal(syn$count, round(syn$count))  # integer-valued original -> whole numbers
})


test_that("seeded synthesis on long numeric columns is deterministic and RNG-neutral", {
  original <- data.frame(value = round(seq(0.001, 1.500, length.out = 1500), 3))
  spec <- synth_spec(purpose = "demo", n = 1500, seed = 42)

  set.seed(999)
  before <- .Random.seed
  syn1 <- synthesize_data(original, spec, engine = "internal")
  after_first <- .Random.seed
  syn2 <- synthesize_data(original, spec, engine = "internal")
  after_second <- .Random.seed

  expect_equal(syn1, syn2)
  expect_equal(before, after_first)
  expect_equal(before, after_second)
})
