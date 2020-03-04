library(memr)

test_that("synonym_task finds good answers for identical embeddings", {
  a1 <- c(1, 0, 0)
  a2 <- c(1, 0, 0)
  b1 <- c(0, 1, 0)
  b2 <- c(0, 1, 0)
  term_vec <- rbind(a1, a2, b1, b2)

  terms = list("a1", "a2")
  expect_equal(synonym_task(term_vec, n = 1, terms)[[1]], 1)

  terms = list(c("a1", "b1"), c("a2", "b2"))
  expect_equal(synonym_task(term_vec, n = 1, terms)[[1]], 1)
})

test_that("synonym_task works well on improper synonyms", {
  a1 <- c(1, 0)
  a2 <- c(0, 1)
  b1 <- c(1, 1)
  b2 <- c(1, 2)
  term_vec <- rbind(a1, a2, b1, b2)

  terms = list(c("a1", "b1"), c("a2", "b2"))
  expect_identical(synonym_task(term_vec, n = 1, terms)[[2]][, 4],
                   c("FALSE", "TRUE", "FALSE", "TRUE"))
})

test_that("synonym_task returns higher accuracy for greater n", {
  a1 <- c(1, 0)
  a2 <- c(0, 1)
  b1 <- c(1, 1)
  b2 <- c(1, 2)
  term_vec <- rbind(a1, a2, b1, b2)
  terms = list(c("a1", "b1"), c("a2", "b2"))
  expect_true(synonym_task(term_vec, n = 1, terms)[[1]] <=
                synonym_task(term_vec, n = 2, terms)[[1]])
  expect_true(synonym_task(term_vec, n = 2, terms)[[1]] <=
                synonym_task(term_vec, n = 3, terms)[[1]])
})

test_that("analogy_task finds good answers for identical differences", {
  a1 <- c(2, 0, 0)
  a2 <- c(1, 0, 0)
  b1 <- c(2, 1, 0)
  b2 <- c(1, 1, 0)
  term_vec <- rbind(a1, a2, b1, b2)

  terms = list(c("a1", "b1"), c("a2", "b2"))
  expect_equal(analogy_task(term_vec, n = 1, terms)[[1]], 1)
})

test_that("analogy_task works well on improper analogies", {
  a1 <- c(1, 0)
  a2 <- c(0, 1)
  b1 <- c(1, 1)
  b2 <- c(1, 2)
  term_vec <- rbind(a1, a2, b1, b2)

  terms = list(c("a1", "b1"), c("a2", "b2"))
  expect_identical(synonym_task(term_vec, n = 1, terms)[[2]][, 4],
                   c("FALSE", "TRUE", "FALSE", "TRUE"))
})

test_that("synonym_task returns higher accuracy for greater n", {
  a1 <- c(1, 0)
  a2 <- c(0, 1)
  b1 <- c(1, 1)
  b2 <- c(1, 2)
  term_vec <- rbind(a1, a2, b1, b2)
  terms = list(c("a1", "b1"), c("a2", "b2"))
  expect_true(synonym_task(term_vec, n = 1, terms)[[1]] <=
                synonym_task(term_vec, n = 2, terms)[[1]])
  expect_true(synonym_task(term_vec, n = 2, terms)[[1]] <=
                synonym_task(term_vec, n = 3, terms)[[1]])
})


inter_term_vectors <- embed_terms(interviews, n_iter = 2,
                                  term_count_min = 1L)

test_that("evaluate_term_embeddings returns good matrix for n = 1", {
  terms_pairs <- list(
    anatomic = list(c("rhinitis", "fever"), c("cough", "thyroid"))
  )

  expect_identical(rownames(evaluate_term_embeddings(inter_term_vectors,
                                                     terms_pairs,
                                                     n_max = 1L)),
                   c("anatomic", "MEAN"))

})

test_that("evaluate_term_embeddings returns good matrix for two elements synonym", {
  terms_pairs <- list(
    synonym = list("rhinitis", "fever"),
    synonym = list("cough", "thyroid")
  )

  expect_identical(rownames(evaluate_term_embeddings(inter_term_vectors, terms_pairs)),
                   c("synonym", "synonym", "MEAN"))

})

test_that("evaluate_term_embeddings returns good matrix for no element synonym", {

  terms_pairs <- list(
    anatomic = list(c("rhinitis", "fever"), c("cough", "thyroid"))
  )

  expect_identical(rownames(evaluate_term_embeddings(inter_term_vectors, terms_pairs)),
                   c("anatomic", "MEAN"))

})

test_that("evaluate_term_embeddings returns good matrix for one element synonym", {

  terms_pairs <- list(
    anatomic = list(c("rhinitis", "fever"), c("cough", "thyroid")),
    synonym = list("rhinitis", "fever")
  )
  expect_identical(rownames(evaluate_term_embeddings(inter_term_vectors, terms_pairs)),
                   c("anatomic", "synonym", "MEAN"))

})

test_that("visualize_analogies throws an error when gets empty words", {
  terms1 = NULL
  terms2 = NULL
  expect_error(visualize_analogies(inter_term_vectors, list(terms1, terms2)))
})

test_that("visualize_analogies throws an error when gets different number of words from pairs", {
  terms1 = c("rhinitis", "fever")
  terms2 =  c("cough", "thyroid", "eye")
  expect_error(visualize_analogies(inter_term_vectors, list(terms1, terms2)),
               "The lists of terms must have the same length.")
  expect_error(visualize_analogies(inter_term_vectors, list(terms1, terms2),
                                   find_analogies = TRUE),
               "The lists of terms must have the same length.")
})

test_that("visualize_analogies throws an error when given words have not got embeddings", {
  terms1 = c("rhinitis", "fever")
  terms2 = c("aaaa", "bbbb")

  expect_error(visualize_analogies(inter_term_vectors, list(terms1, terms2)),
               "Analogy terms have not got embeddings, the plot cannot be generated.")

  expect_error(visualize_analogies(inter_term_vectors, list(terms1, terms2),
                                   find_analogies = TRUE),
               "Analogy terms have not got embeddings, the plot cannot be generated.")
})

test_that("visualize_analogies gives a message when no analogy is fulfilled", {
  terms1 = c("rhinitis", "fever")
  terms2 = c("eye", "cough")
  found_analogies <- analogy_task(inter_term_vectors, n = 1, list(terms1, terms2))$questions
  if (sum(found_analogies[, 6] == "TRUE") == 0) {
    expect_message(visualize_analogies(inter_term_vectors, list(terms1, terms2),
                        find_analogies = TRUE, n = 1),
                   "There are no fulfilled analogies. Trying to plot all words...")
  }
})

examination_term_vectors <-
  embed_terms(merged_terms = examinations,
              embedding_size = 3,
              term_count_min = 1L)

test_that("visualize_analogies saves plots to the specified path", {

  path <- "/tmp/plot.pdf"
  visualize_analogies(examination_term_vectors,
                      terms_pairs_test$person,
                      save = TRUE,
                      path_to_save = path)
  expect_true(file.exists(path))

  path <- "/tmp/plot.png"
  visualize_analogies(examination_term_vectors,
                      terms_pairs_test$person,
                      save = TRUE,
                      path_to_save = path)
  expect_true(file.exists(paste0(path, ".pdf")))
})

test_that("visualize_analogies throws an error when gets improper path", {
  if (!dir.exists("/aaa")) {
    path <- "/aaa/plot.pdf"
    expect_error(visualize_analogies(examination_term_vectors,
                                       terms_pairs_test$person,
                                       save = TRUE,
                                       path_to_save = path))
  }
})
