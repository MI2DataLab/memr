if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Visualize Embeddings of Terms
#'
#' Visualize given embeddings of terms of specified categories.
#' The visualization is generated on the 2D plane by t-SNE algorithm
#' or by PCA (two main components) and plotted with ggplot.
#' If t-SNE cannot be generated (because for example
#' perplexity is too large for the number of samples), there are plotted
#' two main components from PCA.
#' The plot can be optionally saved to the given PDF file.
#'
#' @param term_table A data frame with columns:
#'     \describe{
#'       \item{\code{term}}{A medical term (elements in this column do not have to be unique)}
#'       \item{\code{category}}{A category of the term}
#'     }
#' @param term_vectors A matrix of embeddings of the terms
#' @param category A character vector of categories of the terms to be visualized
#' @param method One of "tsne" (default) or "pca" - a method of generating the plot
#' @param save A logical indicating if the plot should be saved to the file
#' @param path_to_save An optional string of the path to the target PDF file
#'
#' @return A generated plot of embeddings.
#'
#' @examples
#' inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
#'   term_count_min = 1L)
#'
#' visualize_term_embeddings(terms_categories, inter_term_vectors,
#'                           unique(as.character(terms_categories$category)))
#' visualize_term_embeddings(terms_categories, inter_term_vectors,
#'                           c("anatomic"), method = "pca")
#'
#' @export
visualize_term_embeddings <- function(term_table, term_vectors, category,
                                      method = "tsne",
                                      save = FALSE, path_to_save) {
  if (!("term" %in% colnames(term_table))) {
    stop("term_table must contain the column named 'term'")
  }
  if (!("category" %in% colnames(term_table))) {
    stop("term_table must contain the column named 'category'")
  }
  plot_terms <- unique(unlist(term_table$term[term_table$category %in% category]))

  plot_vectors <- term_vectors[intersect(rownames(term_vectors), plot_terms),]

  label_size <- min(120 / sqrt(nrow(plot_vectors)), 15)
  point_size <- label_size / 3
  cat_paste <- paste0(category, collapse = ", ")
  set.seed(28-12-2019)
  plot_data <- NULL
  if (method == "tsne") {
    plot_data <- tryCatch(
      {
        tsne <- Rtsne::Rtsne(plot_vectors, dims = 2, initial_dims = ncol(term_vectors),
                             check_duplicates = FALSE, max_iter = 1000)
        list(data.frame(tsne$Y), "tSNE")
      },
      error = function(cond) {
        message("Could not generate tSNE plot.")
        message("Here's the original error message:")
        message(cond)
        message("\nTrying to generate PCA plot...")
        return(NULL)
      }
    )
  }
  if (is.null(plot_data)) {
    pc <- stats::prcomp(t(plot_vectors))
    plot_data <- list(data.frame(pc$rotation[, c(1, 2)]), "PCA")
  }

  g <- ggplot2::ggplot(plot_data[[1]], ggplot2::aes(plot_data[[1]][, 1],
                                                    plot_data[[1]][, 2]),
                       shape = TRUE, size = 5, label = FALSE) +
    ggplot2::ggtitle(paste(plot_data[[2]], "visualization for category:", cat_paste)) +
    ggrepel::geom_label_repel(label = rownames(plot_vectors), size = label_size) +
    ggplot2::geom_point(size = point_size)

  if (save) {
    if (substr(path_to_save, nchar(path_to_save) - 3,
              nchar(path_to_save)) != ".pdf") {
      path_to_save <- paste0(path_to_save, ".pdf")
    }

    grDevices::pdf(file = path_to_save, width = 50, height = 40)
    g <- g + ggplot2::theme(plot.title = ggplot2::element_text(size = 40, face = "bold"))
    graphics::plot(g)
    grDevices::dev.off()
    message("Successfully saved a plot.")
  }
  g
}

#' Visualize Embeddings of Visits
#'
#' Visualize given embeddings of visits of specified specialties of doctors.
#' The visualization is generated on the 2D plane by t-SNE algorithm
#' or by PCA (two main components) and plotted with ggplot.
#' If t-SNE cannot be generated (because for example
#' perplexity is too large for the number of samples), there are plotted
#' two main components from PCA.
#' Points representing the visits can be colored by doctor ID,
#' the first letter of ICD-10 code or by cluster number.
#' The plot can be optionally saved to the given PDF file.
#'
#' @param visit_vectors A matrix of embeddings of visits
#' @param visit_table A data frame with columns:
#'     \describe{
#'       \item{\code{visit_id}}{}
#'       \item{\code{icd10}}{(Optional, if \code{color_by == "icd10"}): ICD-10 code of the visit}
#'       \item{\code{doctor_id}}{(Optional, if \code{color_by == "doctor"}): doctor's ID}
#'       \item{\code{specialties}}{Specialties of doctors, separated by \code{", "}}
#'     }
#' @param method One of \code{"tsne"} (default) or \code{"pca"} - a method of generating the plot
#' @param color_by A string, one of \code{{"doctor", "icd10", "cluster"}}
#' @param spec A specialty to be plotted (optional, required if \code{color_by != "clusters"})
#' @param clusters An output of the function \link{cluster_visits}
#' (optional, required if \code{color_by == "clusters"})
#' @param n_doctors A number of doctors to be colored, the rest will be grey
#' (optional, required if \code{color_by == "doctor"}), default: 5
#' @param n_codes A number of codes to be colored, the rest will be grey
#' (optional, required if \code{color_by == "icd10"}), default: 9
#' @param save A logical indicating if the plot should be saved to the file
#' @param path_to_save An optional string of the path to the target PDF file
#'
#' @return A generated plot of embeddings.
#'
#' @examples
#' inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
#'                                   term_count_min = 1L)
#' exam_term_vectors <- embed_terms(examinations, embedding_size = 10L,
#'                                  term_count_min = 1L)
#' visits_vectors <- embed_list_visits(interviews,
#'                                     examinations, inter_term_vectors,
#'                                     exam_term_vectors)
#'
#' visualize_visit_embeddings(visits_vectors, visits, "tsne",
#'                            color_by = "doctor", spec = "internist", n_doctors = 10)
#' visualize_visit_embeddings(visits_vectors, visits, "pca",
#'                            color_by = "doctor", spec = "internist", n_doctors = 9)
#' visualize_visit_embeddings(visits_vectors, visits,
#'                            color_by = "icd10", spec = "internist", n_codes = 4)
#'
#' clusters <- cluster_visits(visits_vectors, visits,
#'                            spec = "internist", cluster_number = 2)
#' visualize_visit_embeddings(visits_vectors, visits,
#'                            color_by = "cluster",
#'                            clusters = clusters)
#'
#' @export
visualize_visit_embeddings <- function(visits_vectors, visit_table,
                                       method = "tsne",
                                       color_by, spec = NULL, clusters,
                                       n_doctors = 5L, n_codes = 9L, save = FALSE,
                                       path_to_save) {
  if (!(color_by %in% c("doctor", "icd10", "cluster"))) {
    stop("color_by must be one of: 'doctor', 'icd10', 'cluster'")
  }
  if (!("visit_id" %in% colnames(visit_table))) {
    stop("visit_table must contain the column named 'visit_id'")
  }
  if (!("specialties" %in% colnames(visit_table))) {
    stop("visit_table must contain the column named 'specialties'")
  }
  if ((color_by == "icd10") && !("icd10" %in% colnames(visit_table))) {
    stop("visit_table must contain the column named 'icd10' if color_by == 'icd10'")
  }
  if ((color_by == "doctor") && !("doctor_id" %in% colnames(visit_table))) {
    stop("visit_table must contain the column named 'doctor_id' if color_by == 'doctor'")
  }
  if (!(color_by == "cluster") && (is.null(spec))) {
    stop("please specify a specialty")
  }
  if (class(visit_table$specialties) != "character") {
    stop("the class of the column 'specialties' must be 'character'")
  }
  if (sum(duplicated(visit_table$visit_id)) > 0) {
    stop("column `visit_id` must contain unique values")
  }

  visit_table <- visit_table[!duplicated(visit_table), ]
  visits_vectors[is.na(visits_vectors)] <- 0

  if (color_by == "cluster") {
    visits_ids <- intersect(names(clusters$cluster),
                            rownames(visits_vectors))
    spec_ids <- visits_ids
    if (is.null(spec)) {
      spec <- "given in the clustering"
    }
    color_label <- as.factor(clusters$cluster)
  } else {
    spec_ids <- visit_table$visit_id[sapply(1:nrow(visit_table), function(i)
      spec %in% unlist(strsplit(visit_table$specialties[i], split = ", ")))]
    spec_ids <- intersect(rownames(visits_vectors), as.character(spec_ids))

    if (color_by == "doctor") {
      doctor_ids <- visit_table$doctor_id[visit_table$visit_id %in% spec_ids]
      doctor_ids_table <- sort(table(doctor_ids), decreasing = TRUE)
      color_doctors <- names(doctor_ids_table)[1:n_doctors]

      # ids of visits to be colored, the rest will be grey
      visits_ids <- intersect(visit_table$visit_id[visit_table$doctor_id %in% color_doctors],
                              spec_ids)
      color_label <- as.factor(visit_table$doctor_id[visit_table$visit_id %in% visits_ids])
    }
    if (color_by == "icd10") {
      codes_ids <- substr(visit_table$icd10[visit_table$visit_id %in% spec_ids],
                          1, 1)
      codes_ids_table <- sort(table(codes_ids), decreasing = TRUE)
      color_codes <- names(codes_ids_table)[1:n_codes]

      visits_ids <- intersect(visit_table$visit_id[
        substr(visit_table$icd10, 1, 1) %in% color_codes
      ], spec_ids)
      color_label <- substr(visit_table$icd10[visit_table$visit_id %in% visits_ids], 1, 1)
    }
  }

  set.seed(28-12-2019)
  plot_data <- NULL
  if (method == "tsne") {
    plot_data <- tryCatch(
      {
        tsne <- Rtsne::Rtsne(visits_vectors[spec_ids, ], dims = 2,
                             initial_dims = ncol(visits_vectors), check_duplicates = FALSE,
                             max_iter = 500)
        list(data.frame(tsne$Y), "tSNE")
      },
      error = function(cond) {
        message("Could not generate tSNE plot.")
        message("Here's the original error message:")
         message(cond)
       message("\nTrying to generate PCA plot...")
        return(NULL)
      }
    )
  }
  if (is.null(plot_data)) {
    pc <- stats::prcomp(t(visits_vectors[spec_ids, ]))
    plot_data <- list(data.frame(pc$rotation[, c(1, 2)]), "PCA")
  }
  plot_tool <- plot_data[[2]]
  plot_data <- plot_data[[1]]
  rownames(plot_data) <- spec_ids
  plot_data_color <- plot_data[visits_ids, ]
  plot_data_grey <- plot_data[setdiff(spec_ids, visits_ids), ]
  g <- ggplot2::ggplot(plot_data, ggplot2::aes(x = plot_data[,1],
                                               y = plot_data[, 2])) +
         ggplot2::geom_point(data = plot_data_color,
                             ggplot2::aes(x = plot_data_color[, 1],
                                          y = plot_data_color[, 2],
                                          colour = color_label), size = 1) +
         ggplot2::geom_point(data = plot_data_grey,
                             ggplot2::aes(x = plot_data_grey[, 1],
                                          y = plot_data_grey[, 2]),
                             size = 1, color = "grey") +
         ggplot2::labs(colour = color_by) +
         ggplot2::ggtitle(paste0(plot_tool, " embeddings of specialty: '", spec, "'")) +
         ggplot2::xlab(ggplot2::element_blank()) +
         ggplot2::ylab(ggplot2::element_blank())

  if (length(unique(color_label)) < 10) g <- g +
    ggplot2::scale_colour_brewer(palette = "Set1")

  if (save) {
    if (substr(path_to_save, nchar(path_to_save) - 3,
               nchar(path_to_save)) != ".pdf") {
      path_to_save <- paste0(path_to_save, ".pdf")
    }
    grDevices::pdf(file = path_to_save, width = 7, height = 5)
    graphics::plot(g)
    grDevices::dev.off()
    message("Successfully saved a plot.")
  }
  g
}

#' Generate and Visualize Embeddings of ICD-10 Codes
#'
#' Generate and visualize embeddings of ICD-10 codes.
#' An embedding of the code is the simple average of embeddings
#' of all visits assigned by this code.
#' The visualization is generated on the 2D plane by t-SNE algorithm
#' and plotted with ggplot.
#' If t-SNE cannot be generated (because for example
#' perplexity is too large for the number of samples), there are plotted
#' two main components from PCA.
#' The plot can be optionally saved to the given PDF file.
#'
#' @param visit_vectors A matrix of embeddings of visits
#' @param visit_table A data frame with columns:
#'     \describe{
#'       \item{\code{visit_id}}{}
#'       \item{\code{icd10}}{ICD-10 code of the visit}
#'     }
#' @param method One of "tsne" (default) or "pca" - a method of generating the plot
#' @param save A logical indicating if the plot should be saved to the file
#' @param path_to_save An optional string of the path to the target PDF file
#'
#' @return A generated plot of embeddings.
#'
#' @examples
#' inter_term_vectors <- embed_terms(interviews, embedding_size = 10L,
#'                                   term_count_min = 1L)
#' exam_term_vectors <- embed_terms(examinations, embedding_size = 10L,
#'                                  term_count_min = 1L)
#' visits_vectors <- embed_list_visits(interviews,
#'                                     examinations, inter_term_vectors, exam_term_vectors)
#'
#' visualize_icd10(visits_vectors, visits)
#'
#' @export
visualize_icd10 <- function(visits_vectors, visit_table,
                            method = "tsne",
                            save = FALSE, path_to_save) {

  if (!("visit_id" %in% colnames(visit_table))) {
    stop("visit_table must contain the column named 'visit_id'")
  }
  if (!("icd10" %in% colnames(visit_table))) {
    stop("visit_table must contain the column named 'icd10'")
  }
  if (sum(duplicated(visit_table$visit_id)) > 0) {
    stop("column `visit_id` must contain unique values")
  }

  visits_vectors[is.na(visits_vectors)] <- 0
  codes <- sort(unique(visit_table$icd10))
  codes_embeddings_mean <- matrix(0, ncol = ncol(visits_vectors), nrow = length(codes))
  rownames(codes_embeddings_mean) <- codes
  for (code in codes) {
    ids_visits <- intersect(visit_table$visit_id[visit_table$icd10 == code],
                            rownames(visits_vectors))
    if (length(ids_visits) > 0) {
      code_vectors <- visits_vectors[as.character(ids_visits), ]
      if (!is.array(code_vectors)) {
        codes_embeddings_mean[code,] <- code_vectors
      } else {
        codes_embeddings_mean[code,] <- apply(as.matrix(code_vectors), 2, mean)
      }
    }
  }

  codes_embeddings_mean <- codes_embeddings_mean[rowSums(abs(codes_embeddings_mean)) > 0, ]
  codes <- rownames(codes_embeddings_mean)
  codes_X <- substr(codes, 1, 1)
  set.seed(28-12-2019)
  plot_data <- NULL
  if (method == "tsne") {
    plot_data <- tryCatch(
      {
        tsne <- Rtsne::Rtsne(codes_embeddings_mean, dims = 2,
                             initial_dims = ncol(visits_vectors),
                             check_duplicates = FALSE, max_iter = 500)
        list(data.frame(tsne$Y), "tSNE")
      },
      error = function(cond) {
        message("Could not generate tSNE plot.")
        message("Here's the original error message:")
        message(cond)
        message("\nTrying to generate PCA plot...")
        return(NULL)
      }
    )
  }
  if (is.null(plot_data)) {
    pc <- stats::prcomp(t(codes_embeddings_mean))
    plot_data <- list(data.frame(pc$rotation[, c(1, 2)]), "PCA")
  }
  categories_count <- length(unique(codes_X))

  colors <- grDevices::rainbow(categories_count)
  names(colors) <- unique(codes_X)

  t <- table(visit_table$icd10)[codes]
  sizes <- log(t) + 0.7

  g <- ggplot2::ggplot(plot_data[[1]], ggplot2::aes(plot_data[[1]][, 1],
                                                    plot_data[[1]][, 2]),
                       shape = TRUE, size = 5, label = FALSE) +
         ggplot2::geom_text(label = codes, size = sizes, colour = colors[codes_X]) +
         ggplot2::ggtitle(paste(plot_data[[2]], "ICD-10 embeddings")) +
         ggplot2::xlab(ggplot2::element_blank()) + ggplot2::ylab(ggplot2::element_blank())

  if (save) {
    if (substr(path_to_save, nchar(path_to_save) - 3,
               nchar(path_to_save)) != ".pdf") {
      path_to_save <- paste0(path_to_save, ".pdf")
    }
    grDevices::pdf(file = path_to_save, width = 25, height = 20)
    g <- g + ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"))
    graphics::plot(g)
    grDevices::dev.off()
    message("Successfully saved a plot.")
  }
  g
}
