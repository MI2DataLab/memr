library(memr)

inter_term_vectors <- embed_terms(interviews, n_iter = 2L, term_count_min = 1L)

test_that("visualize_term_embeddings saves plots to the specified path", {
  path <- "/tmp/plot.pdf"
  visualize_term_embeddings(term_table = terms_categories,
                            term_vectors = inter_term_vectors,
                            category = unique(terms_categories$category),
                            method = "pca",
                            save = TRUE, path_to_save = path)
  expect_true(file.exists(path))

  path <- "/tmp/plot.png"
  visualize_term_embeddings(term_table = terms_categories,
                            term_vectors = inter_term_vectors,
                            category = unique(terms_categories$category),
                            method = "pca",
                            save = TRUE, path_to_save = path)
  expect_true(file.exists(paste0(path, ".pdf")))
})

test_that("visualize_term_embeddings throws an error when gets improper path", {
  if (!dir.exists("/aaa")) {
    path <- "/aaa/plot.pdf"
    expect_error(visualize_term_embeddings(terms_categories, inter_term_vectors,
                                           method = "pca",
                                           unique(terms_categories$category),
                                           save = TRUE, path_to_save = path))
  }
})

inter_term_vectors <- embed_terms(interviews, embedding_size = 5L,
                                  n_iter = 2L, term_count_min = 1L)
exam_term_vectors <- embed_terms(examinations, embedding_size = 5L,
                                 n_iter = 2L, term_count_min = 1L)
visits_vectors <- embed_list_visits(interviews, examinations,
                                    inter_term_vectors, exam_term_vectors)

test_that("visualize_visit_embeddings saves plots to the specified path", {
  path <- "/tmp/plot.pdf"
  visualize_visit_embeddings(visits_vectors, visits,
                             color_by = "doctor", spec = "internist",
                             method = "pca",
                             n_doctors = 9, save = TRUE,
                             path_to_save = path)
  expect_true(file.exists(path))

  path <- "/tmp/plot.png"
  visualize_visit_embeddings(visits_vectors, visits,
                             color_by = "doctor", spec = "internist",
                             method = "pca",
                             n_doctors = 9, save = TRUE,
                             path_to_save = path)
  expect_true(file.exists(paste0(path, ".pdf")))
})

test_that("visualize_visit_embeddings works for clusters and no 'spec'", {
  clusters <- cluster_visits(visits_vectors, visits,
                             spec = "internist",
                             cluster_number = 2)
  path <- "/tmp/plot.pdf"
  visualize_visit_embeddings(visits_vectors, visits,
                             color_by = "cluster",
                             method = "pca",
                             clusters = clusters, save = TRUE,
                             path_to_save = path)
  expect_true(file.exists(path))
})

test_that("visualize_visit_embeddings throws an error when gets improper path", {
  if (!dir.exists("/aaa")) {
    path <- "/aaa/plot.pdf"
    expect_error(visualize_visit_embeddings(visits_vectors, visits,
                               color_by = "doctor", spec = "internist",
                               method = "pca",
                               n_doctors = 9, save = TRUE,
                               path_to_save = path))
  }
})

test_that("visualize_visit_embeddings throws an error when some arguments are lacking", {
  expect_error(visualize_visit_embeddings(visits_vectors, visits,
                                          method = "pca",
                                           spec = "internist"))

  expect_error(visualize_visit_embeddings(visits_vectors, visits,
                                          method = "pca",
                                          color_by = "doctor"),
               "please specify a specialty")

  expect_error(visualize_visit_embeddings(visits_vectors, visits,
                                          method = "pca",
                                          spec = "nicnierobienie",
                                          color_by = "doctor"))

  expect_error(visualize_visit_embeddings(visits_vectors, visits,
                                          method = "pca",
                                          spec = "internist",
                                          color_by = "visit"),
               "color_by must be one of: 'doctor', 'icd10', 'cluster'")
})

test_that("visualize_visit_embeddings throws an error when the columns of visit_table are wrong", {
  visits2 <- visits
  colnames(visits2)[2] <- "icd"
  expect_error(visualize_visit_embeddings(visits_vectors, visits2,
                                          method = "pca",
                                          spec = "internist",
                                          color_by = "icd10"),
               "visit_table must contain the column named 'icd10' if color_by == 'icd10'")

  visits2 <- visits
  colnames(visits2)[3] <- "doctor"
  expect_error(visualize_visit_embeddings(visits_vectors, visits2,
                                          method = "pca",
                                          spec = "internist",
                                          color_by = "doctor"),
               "visit_table must contain the column named 'doctor_id' if color_by == 'doctor'")

  visits2 <- visits
  colnames(visits2)[4] <- "spec"
  expect_error(visualize_visit_embeddings(visits_vectors, visits2,
                                          method = "pca",
                                          spec = "internist",
                                          color_by = "doctor"),
               "visit_table must contain the column named 'specialties'")
  expect_error(visualize_visit_embeddings(visits_vectors, visits2,
                                          method = "pca",
                                          spec = "internist",
                                          color_by = "icd10"),
               "visit_table must contain the column named 'specialties'")

  visits2 <- visits
  visits2[, 4] <- as.factor(visits2[, 4])
  expect_error(visualize_visit_embeddings(visits_vectors, visits2,
                                          method = "pca",
                                          spec = "internist",
                                          color_by = "doctor"),
               "the class of the column 'specialties' must be 'character'")
})


test_that("visualize_visit_embeddings throws an error when the column visit_id contains duplications", {
  visits2 <- visits
  visits2$visit_id[1:2] <- 1
  expect_error(visualize_visit_embeddings(visits_vectors, visits2,
                                          method = "pca",
                                          spec = "internist",
                                          color_by = "icd10"),
               "column `visit_id` must contain unique values")
})


test_that("visualize_icd10 saves plots to the specified path", {
  path <- "/tmp/plot.pdf"
  visualize_icd10(visits_vectors, visits,
                  method = "pca",
                  save = TRUE,
                  path_to_save = path)
  expect_true(file.exists(path))

  path <- "/tmp/plot.png"
  visualize_icd10(visits_vectors, visits,
                  method = "pca",
                  save = TRUE,
                  path_to_save = path)
  expect_true(file.exists(paste0(path, ".pdf")))
})

test_that("visualize_icd10 throws an error when gets improper path", {
  if (!dir.exists("/aaa")) {
    path <- "/aaa/plot.pdf"
    expect_error(visualize_icd10(visits_vectors, visits,
                                 method = "pca",
                                 save = TRUE,
                                 path_to_save = path))
  }
})
