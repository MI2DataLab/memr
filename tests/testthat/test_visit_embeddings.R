library(memr)

test_that("embed_description gives an arithmetic mean", {
  term_vec <- diag(3)
  rownames(term_vec) <- c("a", "b", "c")
  expect_equal(embed_description("a", term_vec),
               c(1, 0, 0))
  expect_equal(embed_description("a, a", term_vec),
               c(1, 0, 0))
  expect_equal(embed_description("a, d", term_vec),
               c(1, 0, 0))
  expect_equal(embed_description("a, b", term_vec),
               c(0.5, 0.5, 0))
  expect_equal(embed_description("a, b, a", term_vec),
               c(0.5, 0.5, 0))
  expect_equal(embed_description("a, b, d", term_vec),
               c(0.5, 0.5, 0))
  expect_equal(embed_description("a, b, c", term_vec),
               c(1/3, 1/3, 1/3))
  expect_equal(embed_description("a, a, a, b, b, c, c, d, e, f", term_vec),
               c(1/3, 1/3, 1/3))
})

test_that("embed_description produces NA", {
  term_vec <- diag(3)
  rownames(term_vec) <- c("a", "b", "c")
  expect_identical(embed_description("", term_vec),
               rep(NA, 3))
  expect_identical(embed_description("d", term_vec),
                   rep(NA, 3))
  expect_identical(embed_description("d, e", term_vec),
                   rep(NA, 3))
})
