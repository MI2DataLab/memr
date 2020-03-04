if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Generate Embeddings of Terms
#'
#' Generate embeddings of terms based on descriptions of visits with
#' using the GloVe algorithm.
#' By default the order of the terms is skipped
#' (all weights in the term coocurrence matrix are equal to 1) and
#' only terms occurring at least 5 times are embedded.
#'
#' @param merged_terms A character vector of visits' descriptions with terms
#'                     separated by \code{", "}
#' @param embedding_size An integer (default: 20)
#' @param term_count_min A minimum number of occurences of term to be embedded (default: 5)
#' @param x_max A \code{x_max} parameter of GloVe, see \code{?text2vec::GlobalVectors} (default: 10)
#' @param n_iter A number of epochs of GloVe (default: 15)
#'
#' @return A matrix of embeddings of the terms.
#' @examples
#' inter_term_vectors <- embed_terms(interviews,
#'   term_count_min = 1L)
#' inter_term_vectors
#' inter_term_vectors <- embed_terms(interviews,
#'   term_count_min = 1L, embedding_size = 10L)
#' inter_term_vectors
#' inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
#' term_count_min = 1, n_iter = 50, x_max = 20)
#' inter_term_vectors
#'
#' @export
embed_terms <- function(merged_terms,
                        embedding_size = 20L, term_count_min = 5L,
                        x_max = 10L, n_iter = 15L) {
  wiki <- gsub(", ", ",", merged_terms)
  tokens <- text2vec::space_tokenizer(wiki, sep = ",")
  it = text2vec::itoken(tokens, progressbar = FALSE)

  vocab2 <- text2vec::create_vocabulary(it)
  vocab <- text2vec::prune_vocabulary(vocab2, term_count_min = term_count_min)
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  # equal weights for all terms from the description

  tcm <- text2vec::create_tcm(it, vectorizer, skip_grams_window = 1000L, weights = rep(1, 1000))
  set.seed(28-12-2019)
  glove = text2vec::GlobalVectors$new(word_vectors_size = embedding_size,
                                      vocabulary = vocab, x_max = x_max)
  term_vectors <- glove$fit_transform(tcm, n_iter = n_iter)
  return(term_vectors)
}
