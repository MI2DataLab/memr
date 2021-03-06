% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/term_embeddings.R
\name{embed_terms}
\alias{embed_terms}
\title{Generate Embeddings of Terms}
\usage{
embed_terms(merged_terms, embedding_size = 20L, term_count_min = 5L,
  x_max = 10L, n_iter = 15L)
}
\arguments{
\item{merged_terms}{A character vector of visits' descriptions with terms
separated by \code{", "}}

\item{embedding_size}{An integer (default: 20)}

\item{term_count_min}{A minimum number of occurences of term to be embedded (default: 5)}

\item{x_max}{A \code{x_max} parameter of GloVe, see \code{?text2vec::GlobalVectors} (default: 10)}

\item{n_iter}{A number of epochs of GloVe (default: 15)}
}
\value{
A matrix of embeddings of the terms.
}
\description{
Generate embeddings of terms based on descriptions of visits with
using the GloVe algorithm.
By default the order of the terms is skipped
(all weights in the term coocurrence matrix are equal to 1) and
only terms occurring at least 5 times are embedded.
}
\examples{
inter_term_vectors <- embed_terms(interviews,
  term_count_min = 1L)
inter_term_vectors
inter_term_vectors <- embed_terms(interviews,
  term_count_min = 1L, embedding_size = 10L)
inter_term_vectors
inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
term_count_min = 1, n_iter = 50, x_max = 20)
inter_term_vectors

}
