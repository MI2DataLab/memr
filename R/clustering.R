#' @importFrom dplyr %>%
NULL

#' Cluster Visits
#'
#' Cluster visits for given specialty of doctors, based on
#' embeddings of visits. The clustering is performed by
#' the k-means algorithm.
#'
#' @param visits_vectors A matrix of embeddings of visits
#' @param visit_table A data frame with columns:
#'     \describe{
#'       \item{\code{visit_id}}{}
#'       \item{\code{specialties}}{Specialties of doctors, separated by ", "}
#'     }
#' @param spec A specialty to be clustered
#' @param cluster_number A number of desired clusters (default: 5)
#' @param nstart Optionally, \code{nstart} argument of \code{kmeans} (default: 50)
#' @param inter.max Optionally, \code{inter.max} argument of \code{kmeans} (default: 15)
#'
#' @return The result of k-means with: cluster, centers etc.
#'
#' @examples
#' inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
#'                                   term_count_min = 1L)
#' exam_term_vectors <- embed_terms(examinations, embedding_size = 10L,
#'                                  term_count_min = 1L)
#' visits_vectors <- embed_list_visits(interviews,
#'                                     examinations, inter_term_vectors, exam_term_vectors)
#' clusters <- cluster_visits(visits_vectors, visits,
#'                            spec = "internist",
#'                            cluster_number = 2L)
#' clusters$size
#'
#' sum(clusters$withinss)
#' clusters$centers
#'
#' @export
cluster_visits <- function(visits_vectors, visit_table, spec,
                           cluster_number = 5, nstart = 50,
                           iter.max = 15) {
  if (!("visit_id" %in% colnames(visit_table))) {
    stop("visit_table must contain the column named 'visit_id'")
  }
  if (!("specialties" %in% colnames(visit_table))) {
    stop("visit_table must contain the column named 'specialties'")
  }
  if (class(visit_table$specialties) != "character") {
    stop("the class of the column 'specialties' must be 'character'")
  }
  if (sum(duplicated(visit_table$visit_id)) > 0) {
    stop("column `visit_id` must contain unique values")
  }
  spec_ids <- visit_table$visit_id[sapply(1:nrow(visit_table), function(i)
    spec %in% unlist(strsplit(visit_table$specialties[i], split = ", ")))]
  spec_ids <- intersect(rownames(visits_vectors), as.character(spec_ids))
  specialty_visits_vectors <- visits_vectors[spec_ids, ]
  specialty_visits_vectors[is.na(specialty_visits_vectors)] <- 0
  clusters <- stats::kmeans(specialty_visits_vectors, cluster_number,
                     nstart = nstart, iter.max = iter.max)
  return(clusters)
}

#' Assign Visit to the Cluster
#'
#' Assign a given visit to the closest cluster from a given clustering.
#' The function generates the embedding of the visit (by the function \link{embed_visit})
#' and computed the squared Euclidean distance from the clusters' centers.
#'
#' @param visit_description A named two element vector (with names: \code{inter} and \code{exam})
#'  of concatenated terms of interview and examination, separated by ", "
#' @param clusters An output of the function \link{cluster_visits}
#' @param inter_term_vectors A matrix of embeddings of the interview terms
#' @param exam_term_vectors A matrix of embeddings of the examination terms
#'
#' @return A list of:
#'    \item{\code{cl}}{The number of the closest cluster}
#'    \item{\code{distances}}{The vector of distances to each cluster}
#'
#' @examples
#' inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
#'                                   term_count_min = 1L)
#' exam_term_vectors <- embed_terms(examinations, embedding_size = 10L,
#'                                  term_count_min = 1L)
#' visits_vectors <- embed_list_visits(interviews,
#'                                     examinations, inter_term_vectors, exam_term_vectors)
#' clusters <- cluster_visits(visits_vectors, visits,
#'                            spec = "internist",
#'                            cluster_number = 2L)
#' assign_visit_to_cluster(c(inter = "cough, fever, rhinitis", exam = ""),
#'                        clusters, inter_term_vectors, exam_term_vectors)
#' assign_visit_to_cluster(c(inter = "thyroid", exam = ""),
#'                        clusters, inter_term_vectors, exam_term_vectors)
#' assign_visit_to_cluster(c(inter = "cough, fever, rhinitis",
#'                          exam = "patient, heart"),
#'                        clusters, inter_term_vectors, exam_term_vectors)
#'
#' @export
assign_visit_to_cluster <- function(visit_description, clusters,
                                    inter_term_vectors, exam_term_vectors) {
  visit_embedding <- embed_visit(visit_description,
                                 inter_term_vectors,
                                 exam_term_vectors)
  visit_embedding[is.na(visit_embedding)] <- 0
  distances <- apply((clusters$centers - visit_embedding)^2, 1, mean)
  return(list(cl = which.min(distances), distances = distances))
}

#' Get Recommendations for Clusters
#'
#' Get recommendations prescribed by doctors
#' to patients from given clusters. The categories
#' of recommendation terms can be specified.
#'
#' @param recom_descriptions A vector of descriptions of recommendations, named by visits' IDs;
#' terms in the descriptions are separated by ", "
#' @param clusters An output of the function \link{cluster_visits}
#' @param category A vector of categories of the terms to be returned or \code{"all"}
#' (default: \code{"all"})
#' @param recom_table (necessary if \code{category != "all"}) A data frame with columns:
#'     \describe{
#'       \item{\code{term}}{a term of recommendation}
#'       \item{\code{category}}{a category of the term}
#'     }
#'
#' @return A list of data frames. For each cluster there is a data frame with columns:
#'   \item{\code{recommendation}}{A term of recommendation}
#'   \item{\code{count}}{A number of occurrences of the recommentadion in the cluster}
#'   \item{\code{frequency}}{A percentage of visits containing the reccomentation}
#'   Rows are sorted by the \code{frequency}.
#'
#' @examples
#' inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
#'                                   term_count_min = 1L)
#' exam_term_vectors <- embed_terms(examinations, embedding_size = 10L,
#'                                  term_count_min = 1L)
#' visits_vectors <- embed_list_visits(interviews,
#'                                     examinations, inter_term_vectors, exam_term_vectors)
#' clusters <- cluster_visits(visits_vectors, visits,
#'                            spec = "internist",
#'                            cluster_number = 2L)
#'
#' get_cluster_recommendations(recommendations, clusters, recom_table = terms_categories)
#' get_cluster_recommendations(recommendations, clusters, recom_table = terms_categories,
#' category = "anatomic")
#' get_cluster_recommendations(recommendations, clusters, recom_table = terms_categories,
#' category = "all")
#'
#' @export
get_cluster_recommendations <- function(recom_descriptions, clusters,
                                        category = "all",
                                        recom_table) {

  if ((category != "all") && !("term" %in% colnames(recom_table))) {
    stop("recom_table must contain the column named 'term'")
  }
  if ((category != "all") && !("category" %in% colnames(recom_table))) {
    stop("recom_table must contain the column named 'category'")
  }

  cluster_number <- nrow(clusters$centers)
  rec_tables <- lapply(1:cluster_number, function(cl) {
    cluster_ids <- names(clusters$cluster[clusters$cluster == cl])
    cluster_descr <- recom_descriptions[intersect(names(recom_descriptions), cluster_ids)]
    if ((length(cluster_descr) == 0) || (sum(nchar(cluster_descr)) == 0)) {
      data.frame(recommendation = factor(), count = integer(), frequency = numeric())
    }
    else {
      recom <- data.frame(unlist(sapply(strsplit(cluster_descr, split = ", "),
                                     unique)))
      colnames(recom)[1] <- "recommendation"
      recom %>%
        dplyr::group_by(recommendation) %>%
        dplyr::summarise(count = dplyr::n(), frequency = dplyr::n() / clusters$size[cl]) %>%
        dplyr::arrange(dplyr::desc(frequency))
      }
  })
  if (category[1] != "all") {
    recom_list <- unique(unlist(recom_table$term[recom_table$category %in% category]))
    rec_tables <- lapply(rec_tables, function(r)
      r[unlist(r[, "recommendation"]) %in% recom_list, ])
  }
  return(rec_tables)
}
