library(memr)

test_that("embed_terms respects term_count_min", {
  merged_terms <- c("kaszel", "", "kaszel, katar", "gorączka, kaszel",
                    "ból głowy, kaszel, katar", "ból głowy, kaszel, katar",
                    "ból głowy, kaszel, katar")
  expect_identical(sort(rownames(embed_terms(merged_terms = merged_terms,
                                        embedding_size = 5, term_count_min = 3, n_iter = 2))),
                   c("ból głowy", "kaszel", "katar"))
  expect_identical(sort(rownames(embed_terms(merged_terms = merged_terms,
                                    embedding_size = 5, term_count_min = 4, n_iter = 2))),
                   c("kaszel", "katar"))
})

test_that("term embeddings are reproductible", {
  merged_terms <- c("kaszel", "", "kaszel, katar", "gorączka, kaszel",
                    "ból głowy, kaszel, katar", "ból głowy, kaszel, katar",
                    "ból głowy, kaszel, katar")
  emb1 <- embed_terms(merged_terms = merged_terms,
                      embedding_size = 5, term_count_min = 3, n_iter = 2)
  emb2 <- embed_terms(merged_terms = merged_terms,
                      embedding_size = 5, term_count_min = 3, n_iter = 2)
  expect_identical(emb1, emb2)
})
