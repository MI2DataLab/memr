% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analogies.R
\name{synonym_task}
\alias{synonym_task}
\title{Solve Term Synonym Task}
\usage{
synonym_task(term_vectors, n = 1, terms)
}
\arguments{
\item{term_vectors}{A matrix of embeddings of the terms}

\item{n}{A number of neighbors included in the context (default: 1)}

\item{terms}{A list of two character vectors: the first and the second elements of pairs}
}
\value{
A list of:
  \item{accuracy}{An accuracy of the fulfilled questions}
  \item{questions}{A data frame of the all synonym questions and results}
}
\description{
Solve the term synonym task for given embeddings and pairs of synonyms. This task similarly to
\link{analogy_task} validates the quality of the embeddings. Here the terms in pairs are synonyms
-- are related to the same object, so their embeddings should be close to one another.
For every ordered pair of embeddings \code{(term1, term2)} the task is solved if \code{term2}
is in the \code{n} closest terms to \code{term1}.
}
\examples{
term_vectors <- embed_terms(examinations,
  embedding_size = 5L, term_count_min = 1L)
terms <- terms_pairs_test$synonym
synonym_task(term_vectors, 1, terms)
synonym_task(term_vectors, 5, terms)

}
\seealso{
\code{\link{analogy_task}}
}
