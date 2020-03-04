# Choose Embedded Pairs
#
# Choose embedded pairs for given embeddings of terms and pairs of terms.
# This function return only the pairs where both terms have embeddings.
#
# @param term_vectors A matrix of embeddings of the terms
# @param terms A list of two character vectors: the first and the second elements of pairs
#
# @return A list of two character vectors: the first and the second elements of pairs.
#
# @examples
# term_vectors <- embed_terms(interviews, embedding_size = 5L, term_count_min = 1L)
# terms <- terms_pairs_test[[1]]
# terms
# .choose_embedded_pairs(term_vectors, terms)
.choose_embedded_pairs <- function(term_vectors, terms) {
  terms1 <- terms[[1]]
  terms2 <- terms[[2]]
  if (length(terms1) != length(terms2)) stop("The lists of terms must have the same length.")
  embedded_terms1 <- terms1 %in% rownames(term_vectors)
  embedded_terms2 <- terms2 %in% rownames(term_vectors)
  terms1 <- terms1[(1:length(terms1)) * (1 * embedded_terms1) *
                     (1 * embedded_terms2)]
  terms2 <- terms2[(1:length(terms2)) * (1 * embedded_terms1) *
                     (1 * embedded_terms2)]
  list(terms1, terms2)
}

#' Solve Term Analogy Task
#'
#' Solve the term analogy task for given embeddings and pairs of analogies.
#'
#' This function is an implementation of the word analogy task introduced by Mikolov et al. (2013)
#' to validate the quality of word embeddings. For given pairs of terms being in
#' the same relation (e.g. man - woman) there is formed a list of questions
#' by taking all two-element ordered subsets of the pairs. If \code{(term1, term2)} and \code{(term3, term4)}
#' are the embeddings of the selected pairs, we expect that \code{term1 - term2} is close to \code{term3 - term4}.
#' Hence, there is computed the vector \code{e = term1 - term2 + term4} and compared with \code{term3}.
#' The analogy is fulfilled if \code{term3} is in the \code{n} closest terms to \code{e}.
#'
#' @param term_vectors A matrix of embeddings of the terms
#' @param n A number of neighbors included in analogies (default: 1)
#' @param terms A list of two character vectors: the first and the second elements of pairs
#'
#' @return A list of:
#'   \item{accuracy}{An accuracy of the fulfilled analogies}
#'   \item{questions}{A data frame of the all analogy questions and results}
#'
#' @examples
#' term_vectors <- embed_terms(examinations,
#'   embedding_size = 5L, term_count_min = 1L)
#' terms <- terms_pairs_test[[1]]
#' analogy_task(term_vectors, 1, terms)
#' analogy_task(term_vectors, 3, terms)
#' analogy_task(term_vectors, 5, terms)
#'
#' @references Tomas Mikolov, Kai Chen, Greg Corrado, and Jeffrey Dean. 2013.
#'  Efficient estimation of word representations in vector space.
#'  arXiv preprint arXiv:1301.3781.
#'
#' @export
analogy_task <- function(term_vectors, n = 1, terms) {
  terms <- .choose_embedded_pairs(term_vectors, terms)
  terms1 <- terms[[1]]
  terms2 <- terms[[2]]
  if (length(terms1) == 0) {
    warning("Given terms have not got embeddings, NULL will be returned.")
    return(NULL)
  }
  if (length(terms1) == 1) {
    warning("There is only one pair in analogy, NULL wil be returned.")
    return(NULL)
  }
  combinations <- utils::combn(length(terms1), 2)
  combinations <- cbind(combinations, combinations[2:1, ])
  questions <- matrix(ncol = 6, nrow = ncol(combinations))
  questions[, 1] <- terms1[combinations[1, ]]
  questions[, 2] <- terms2[combinations[1, ]]
  questions[, 3] <- terms2[combinations[2, ]]
  questions[, 4] <- terms1[combinations[2, ]]
  questions[, 5] <- sapply(1:nrow(questions), function(i) {
    cos_sim = text2vec::sim2(x = as.matrix(term_vectors),
                             y = matrix(term_vectors[questions[i, 1], ] -
                                          term_vectors[questions[i, 2], ] +
                                          term_vectors[questions[i, 3], ],
                                          nrow = 1),
                             method = "cosine", norm = "l2")
    paste0(labels(utils::head(sort(cos_sim[, 1], decreasing = TRUE), n)),
           collapse = ", ")

  })
  questions[, 6] <- sapply(1:nrow(questions), function(i) {
    questions[i, 4] %in% unlist(strsplit(questions[i, 5], split = ", "))
  })
  colnames(questions) <- c("term1", "term2", "term3", "expected",
                           "closest to term1 - term2 + term3", "correct")

  return(list(accuracy = sum(as.logical(questions[, 6])) / nrow(questions),
              questions = questions))
}

#' Solve Term Synonym Task
#'
#' Solve the term synonym task for given embeddings and pairs of synonyms. This task similarly to
#' \link{analogy_task} validates the quality of the embeddings. Here the terms in pairs are synonyms
#' -- are related to the same object, so their embeddings should be close to one another.
#' For every ordered pair of embeddings \code{(term1, term2)} the task is solved if \code{term2}
#' is in the \code{n} closest terms to \code{term1}.
#'
#' @param term_vectors A matrix of embeddings of the terms
#' @param n A number of neighbors included in the context (default: 1)
#' @param terms A list of two character vectors: the first and the second elements of pairs
#'
#' @return A list of:
#'   \item{accuracy}{An accuracy of the fulfilled questions}
#'   \item{questions}{A data frame of the all synonym questions and results}
#'
#' @examples
#' term_vectors <- embed_terms(examinations,
#'   embedding_size = 5L, term_count_min = 1L)
#' terms <- terms_pairs_test$synonym
#' synonym_task(term_vectors, 1, terms)
#' synonym_task(term_vectors, 5, terms)
#'
#' @export
synonym_task <- function(term_vectors, n = 1, terms) {
  terms <- .choose_embedded_pairs(term_vectors, terms)
  synonym1 <- terms[[1]]
  synonym2 <- terms[[2]]

  if (length(synonym1) == 0) {
    warning("Given terms have not got embeddings, NULL will be returned.")
    return(NULL)
  }
  questions <- matrix(ncol = 4, nrow = length(synonym1) + length(synonym2))
  questions[, 1] <- c(synonym1, synonym2)
  questions[, 2] <- c(synonym2, synonym1)
  questions[, 3] <- sapply(1:nrow(questions), function(i) {
    cos_sim = text2vec::sim2(x = as.matrix(term_vectors),
                             y = matrix(term_vectors[questions[i, 1], ],
                                        nrow = 1),
                             method = "cosine", norm = "l2")
    paste0(labels(sort(cos_sim[, 1], decreasing = TRUE))[1:(n + 1)], collapse = ", ")

  })
  questions[, 4] <- sapply(1:nrow(questions), function(i) {
    questions[i, 2] %in% unlist(strsplit(questions[i, 3], split = ", "))
  })

  colnames(questions) <- c("term1", "expected", "closest to term1", "correct")

  return(list(accuracy = sum(as.logical(questions[, 4])) / nrow(questions),
              questions = questions))
}

#' Evaluate Term Embeddings
#'
#' Evaluate the given term embeddings based on \link{analogy_task} and \link{synonym_task}.
#' The list of test pairs should be given. The \link{analogy_task} is performed
#' for each type of given analogies (initially, there are 7 proposed types of analogies,
#' see \link{terms_pairs_test}). The accuracy is measured for the context size from 1 (the most
#' restrictive) to the given \code{n_max}.
#'
#' @param term_vectors A matrix of embeddings of the terms
#' @param terms_pairs: A list of lists of the test pairs, each list of pairs should
#' contain two vectors of terms; if the list contains the element "synonym", the
#' \link{synonym_task} is performed for these terms, for the rest pairs there
#' is performed \link{analogy_task}
#' @param n_max A maximum number of neighbors included in the context (default: 5)
#'
#' @return A data frame of accuracies of the analogy task and the synonym task
#' for each given list of pairs for the context from 1 to \code{n_max}.
#'
#' @examples
#' term_vectors <- embed_terms(examinations, embedding_size = 5L,
#'   term_count_min = 1L)
#' evaluate_term_embeddings(term_vectors, terms_pairs = terms_pairs_test)
#'
#' @export
evaluate_term_embeddings <- function(term_vectors, terms_pairs = NULL, n_max = 5L) {
  if (is.null(terms_pairs)) terms_pairs <- terms_pairs_test
  results <- matrix(0, nrow = length(terms_pairs) + 1, ncol = n_max + 1)
  colnames(results) <- as.character(c(1:n_max, "MEAN"))
  rownames(results) <- c(names(terms_pairs), "MEAN")
  for (i in which(names(terms_pairs) != "synonym")) {
    for (n in 1:n_max) {
      found_analogies <- analogy_task(term_vectors, n, terms_pairs[[i]])
      results[i, n] <- ifelse(is.null(found_analogies), 0, found_analogies[[1]])
    }
  }
  if (!is.null(terms_pairs[["synonym"]])) {
    for (n in 1:n_max) {
      found_synonyms <- synonym_task(term_vectors, n, terms_pairs[["synonym"]])
      results[which(names(terms_pairs) == "synonym"), n] <- ifelse(is.null(found_synonyms),
        0, found_synonyms[[1]])
    }
  }
  # compute mean results
  if (nrow(results) > 2) {
    results[nrow(results), -ncol(results)] <-
      colMeans(results[-nrow(results), -ncol(results)])
  } else {
    results[nrow(results), -ncol(results)] <-
      results[1, -ncol(results)]
  }
  if (ncol(results) > 2) {
    results[, ncol(results)] <- rowMeans(results[, -ncol(results)])
  } else {
    results[, ncol(results)] <- results[, 1]
  }
  return(results)
}

#' Visualize Term Analogies
#'
#' Visualize analogies for given terms. A visualization of embeddings is
#' two main components from PCA plotted by ggplot on the 2D plane.
#' Additionally, the terms from each given pair are
#' connected to one another. If the embeddings are of good quality, the connection
#' lines should be almost parallel to each other. If \code{find_analogies = TRUE}
#' only the pairs with at least one solved analogy task are plotted.
#'
#' @param term_vectors A matrix of embeddings of terms
#' @param terms A list of two character vectors: the first and the second elements of analogy pairs
#' @param find_analogies A logical indicating if the term analogy task should be performed
#' before the plotting (default: FALSE)
#' @param n An optional number of neighbors included in analogies (default: 5),
#' needed only if \code{find_analogies = TRUE}
#' @param save A logical indicating if the plot should be saved to the file
#' @param path_to_save An optional string of the path to the target PDF file
#'
#' @return A generated plot of embeddings.
#'
#' @examples
#' term_vectors <- embed_terms(examinations, embedding_size = 5L,
#'   term_count_min = 1L)
#' visualize_analogies(term_vectors, terms_pairs_test$person)
#' visualize_analogies(term_vectors, terms_pairs_test$person,
#'   find_analogies = TRUE, n = 10)
#'
#' @export
visualize_analogies <- function(term_vectors, terms, find_analogies = FALSE, n = 5L,
                                save = FALSE, path_to_save) {
  terms <- .choose_embedded_pairs(term_vectors, terms)
  terms1 <- terms[[1]]
  terms2 <- terms[[2]]
  if (length(terms1) == 0) {
    stop("Analogy terms have not got embeddings, the plot cannot be generated.")
  }
  if (find_analogies) {
    found_analogies <- tryCatch(
      {
        analogy_task(term_vectors, n, terms)
      },
      warning = function(cond) {
        message("Computing analogies produced a warning.")
        message("Here's the original warning message:")
        message(cond)
      }
    )

    if (is.null(found_analogies)) {
      message("\nTrying to plot all words...")
    } else {
      found_analogies <- found_analogies$questions
      found_analogies <- found_analogies[found_analogies[, 6] == "TRUE", ]

      if (!is.matrix(found_analogies)) { # there is exactly one fulfilled analogy, so this is a vector
        terms1 <- unique(c(found_analogies[1], found_analogies[4]))
        terms2 <- unique(c(found_analogies[2], found_analogies[3]))
      } else {
        if (nrow(found_analogies) == 0) {
          message("There are no fulfilled analogies. Trying to plot all words...")
        } else {
          terms1 <- unique(c(found_analogies[, 1], found_analogies[, 4]))
          terms2 <- unique(c(found_analogies[, 2], found_analogies[, 3]))
        }
      }
    }
  }
  plot_terms <- c(terms1, terms2)
  plot_vectors <- term_vectors[plot_terms, ]

  label_size <- 5
  point_size <- 3
  set.seed(28-12-2019)
  pc <- stats::prcomp(t(plot_vectors))
  pc <- pc$rotation[, c(1, 2)]
  g <- ggplot2::ggplot(data.frame(pc), ggplot2::aes(pc[, 1], pc[, 2]), shape = TRUE, size = 5, label = FALSE) +
    ggplot2::geom_line(ggplot2::aes(group = c(1:(nrow(plot_vectors) / 2),
                                              1:(nrow(plot_vectors) / 2))),
                       linetype = "dashed", size = 1) +
    ggrepel::geom_label_repel(label = rownames(plot_vectors), size = label_size) +
    ggplot2::geom_point(size = point_size)

  if (save) {
    if (substr(path_to_save, nchar(path_to_save) - 3,
               nchar(path_to_save)) != ".pdf") {
      path_to_save <- paste0(path_to_save, ".pdf")
    }
    grDevices::pdf(file = path_to_save, width = 9, height = 9)
    graphics::plot(g)
    grDevices::dev.off()
    message("Successfully saved a plot.")
  }
  g
}
