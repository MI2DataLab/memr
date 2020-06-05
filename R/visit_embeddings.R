#' Generate an Embedding of a Description
#'
#' Compute a simple average of embeddings of given terms of a description. Only
#' terms that have embeddings are included. If there are not such terms, the vector
#' of NA is returned.
#'
#' @param description A string of concatenated terms extracted from a visit, separated by \code{", "}
#' @param term_vectors A matrix of embeddings of the terms
#'
#' @return A vector of the embedding of the description.
#'
#' @examples
#' term_vectors <- embed_terms(interviews, embedding_size = 10L,
#'   term_count_min = 1L)
#'
#' embed_description("fever", term_vectors)
#' embed_description("fever, cough", term_vectors)
#' embed_description("fever, cough, temperature",
#'                   term_vectors)
#' embed_description("temperature", term_vectors)
#'
#' @seealso \code{\link{embed_visit}}, \code{\link{embed_list_visits}}
#'
#' @export
embed_description <- function(description, term_vectors) {
  embedding_size <- ncol(term_vectors)
  terms <- intersect(rownames(term_vectors),
                     unlist(strsplit(description, split = ", ")))
  if (length(terms) > 0) {
    if (length(terms) == 1) {
      descr_embedding <- term_vectors[terms,]
    } else {
      descr_embedding <- apply(as.matrix(term_vectors[terms,]), 2, mean)
    }
  } else {
    descr_embedding <- rep(NA, embedding_size)
  }
  return(descr_embedding)
}

#' Generate an Embedding of a Visit
#'
#' Generate an embedding of a given visit based on interview and examination descriptions
#' and the embeddings of terms.
#' This function calls \link{embed_description} function and merge the obtained embeddings
#' of the interview and the examination.
#'
#' @param visit_description A named two-element vector (names: "inter" and "exam")
#' of concatenated terms of interview and examination. Terms must be separated by ", ".
#' @param inter_term_vectors A matrix of embeddings of the interview terms
#' @param exam_term_vectors A matrix of embeddings of the examination terms
#'
#' @return A vector of the embedding of the visit.
#'
#' @examples
#' inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
#'  term_count_min = 1L)
#' exam_term_vectors <- embed_terms(examinations, embedding_size = 10L,
#'   term_count_min = 1L)
#'
#' embed_visit(c(inter = "", exam = "heart, woman, eye"),
#'   inter_term_vectors, exam_term_vectors)
#' embed_visit(c(inter = "cough",
#'              exam = "heart, woman, eye"),
#'            inter_term_vectors, exam_term_vectors)
#' embed_visit(c(inter = "cough", exam = ""),
#'            inter_term_vectors, exam_term_vectors)
#' embed_visit(c(inter = "", exam = "objaw chorobowy"),
#'            inter_term_vectors, exam_term_vectors)
#'
#' @seealso \code{\link{embed_description}}, \code{\link{embed_list_visits}}
#'
#' @export
embed_visit <- function(visit_description, inter_term_vectors, exam_term_vectors) {
  inter <- visit_description["inter"]
  if (is.na(inter)) inter <- ""
  exam <- visit_description["exam"]
  if (is.na(exam)) exam <- ""
  inter_descr_vector <- embed_description(inter, inter_term_vectors)
  exam_descr_vector <- embed_description(exam, exam_term_vectors)
  return(c(inter_descr_vector, exam_descr_vector))
}

#' Generate Embeddings of Visits from a List
#'
#' Generate embeddings of given visits, contained in two named vectors. One
#' vector contains the descriptions of interviews and the second -- the
#' descriptions of examinations. This function interatively calls
#' \link{embed_visit} function.
#'
#' @param inter_descriptions A vector of descriptions of interviews, named by visits' IDs;
#' terms in the descriptions are separated by ", "
#' @param exam_descriptions A vector of descriptions of examinations, named by visits' IDs;
#' the visits' IDs do not have to be identical to inter_descriptions
#' @param inter_term_vectors A matrix of embeddings of the interview terms
#' @param exam_term_vectors A matrix of embeddings of the examination terms
#'
#' @return A matrix of embeddings of visits -- in each row there is an embedding of one visit.
#'
#' @examples
#' inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
#'   term_count_min = 1L)
#' exam_term_vectors <- embed_terms(examinations, embedding_size = 10L,
#'   term_count_min = 1L)
#'
#' visits_vectors <- embed_list_visits(interviews,
#'                                     examinations,
#' inter_term_vectors, exam_term_vectors)
#' nrow(visits_vectors)
#' visits_vectors
#' visits_vectors[is.na(visits_vectors)] <- 0
#' D <- as.matrix(dist(visits_vectors))
#'
#' # Two closest visits
#' examinations[rownames(which(D == min(D[D > 0]),
#'                   arr.ind = TRUE))]
#'
#' visits_vectors <- embed_list_visits(interviews, "",
#'   inter_term_vectors, exam_term_vectors)
#' nrow(visits_vectors)
#' visits_vectors
#'
#' @seealso \code{\link{embed_visit}}, \code{\link{embed_description}}
#'
#' @export
embed_list_visits <- function(inter_descriptions, exam_descriptions,
                             inter_term_vectors, exam_term_vectors) {
  visits_ids <- unique(c(names(inter_descriptions), names(exam_descriptions)))
  visit_description <- character(2)
  names(visit_description) <- c("inter", "exam")
  visits_vectors <- sapply(visits_ids, function(id) {
    visit_description["inter"] <- inter_descriptions[id]
    visit_description["exam"] <- exam_descriptions[id]
    embed_visit(visit_description, inter_term_vectors, exam_term_vectors)
  })
  visits_vectors <- visits_vectors[, apply(visits_vectors, 2, function(c)
    sum(is.na(c)) < nrow(visits_vectors))]
  t(visits_vectors)
}
