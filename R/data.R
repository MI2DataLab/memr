#' Medical Visits from Health Centers
#'
#' An example artificial dataset containing
#' the basic information (ICD-10 code, doctor's ID and
#' the specialties of the doctors) of 11
#' medical visits. The dataset is artificial,
#' but it reflects a structure from a real data collected from Polish health centers.
#'
#' @format A data frame with 11 rows and 4 variables:
#' \describe{
#'   \item{visit_id}{ID of the visit}
#'   \item{icd10}{ICD-10 code of the visit}
#'   \item{doctor_id}{ID of the doctor who led the visit}
#'   \item{specialties}{all specialties of the doctor, separated by ", "}
#' }
"visits"

#' Categories of Medical Terms
#'
#' A dataset containing example terms that could be
#' extracted by linguists from the medical free-text records.
#' The procedure of extracting medical terms from the real
#' medical data is described in the paper Dobrakowski et al., 2019.
#' Each term has been assigned to at least one category.
#'
#' @format A data frame with 19 rows and 2 variables:
#' \describe{
#'   \item{term}{A medical term}
#'   \item{category}{A category of the term}
#' }
#'
#' @references Dobrakowski, A., A. Mykowiecka, M. Marciniak, W. Jaworski, and P. Biecek 2019.
#' Interpretable Segmentation of Medical Free-Text
#' Records Based on Word Embeddings. arXiv preprint arXiv:1907.04152.
"terms_categories"

#' Descriptions of Interviews with Patients
#'
#' An artificial dataset containing terms from generated
#' descriptions of interviews with patients.
#' (cf. \link{examinations} and \link{recommendations}).
#'
#' The structure of the visit, i.e. the partition to interview, examination
#' and recommendations is based on real data collected from Polish
#' health centers. The procedure of extracting medical terms from the real
#' medical data is described in the paper Dobrakowski et al., 2019.
#'
#' @format A character vector of legth 10 containing the terms separated by \code{", "}.
#'
#' @references Dobrakowski, A., A. Mykowiecka, M. Marciniak, W. Jaworski, and P. Biecek 2019.
#' Interpretable Segmentation of Medical Free-Text
#' Records Based on Word Embeddings. arXiv preprint arXiv:1907.04152.
#'
#' @seealso \code{\link{examinations}}, \code{\link{recommendations}}
"interviews"

#' Descriptions of Examinations of Patients
#'
#' An artificial dataset containing terms from generated
#' descriptions of examinations of patients
#' (cf. \link{interviews} and \link{recommendations}).
#'
#' The structure of the visit, i.e. the partition to interview, examination
#' and recommendations is based on real data collected from Polish
#' health centers. The procedure of extracting medical terms from the real
#' medical data is described in the paper Dobrakowski et al., 2019.
#'
#' @format A character vector of legth 10 containing the terms separated by \code{", "}.
#'
#' @references Dobrakowski, A., A. Mykowiecka, M. Marciniak, W. Jaworski, and P. Biecek 2019.
#' Interpretable Segmentation of Medical Free-Text
#' Records Based on Word Embeddings. arXiv preprint arXiv:1907.04152.
#'
#' @seealso \code{\link{interviews}}, \code{\link{recommendations}}
"examinations"

#' Descriptions of Recommendations Prescribed to Patients
#'
#' An artificial dataset containing terms from generated
#' descriptions of recommendations prescribed to patients
#' (cf. \link{interviews} and \link{examinations}).
#'
#' The structure of the visit, i.e. the partition to interview, examination
#' and recommendations is based on real data collected from Polish
#' health centers. The procedure of extracting medical terms from the real
#' medical data is described in the paper Dobrakowski et al., 2019.
#'
#' @format A character vector of legth 8 containing the terms separated by \code{", "}.
#'
#' @references Dobrakowski, A., A. Mykowiecka, M. Marciniak, W. Jaworski, and P. Biecek 2019.
#' Interpretable Segmentation of Medical Free-Text
#' Records Based on Word Embeddings. arXiv preprint arXiv:1907.04152.
#'
#' @seealso \code{\link{interviews}}, \code{\link{examinations}}
"recommendations"

#' Test Pairs of Terms to Evaluate Embedding Quality
#'
#' An example dataset containing pairs of analogies of medical terms to evaluate the quality
#' of embedddings of terms by functions \link{analogy_task} and \link{synonym_task}.
#'
#' Assessing the quality of real-data embeddings was performed by 7 types of analogies, described
#' in the paper Dobrakowski et al., 2019.
#'
#' @format A list with elements:
#' \describe{
#'   \item{spec}{Terms with relation: specialty -- body part (e.g. "cardiologist" -- "heart");
#'     a list of two character vectors of length 2}
#'   \item{person}{Terms with relation: man -- woman;
#'     a list of two character vectors of length 2}
#'   \item{synonym}{Synonym terms;
#'     a list of two character vector of length 1}
#' }
#'
#' @references Dobrakowski, A., A. Mykowiecka, M. Marciniak, W. Jaworski, and P. Biecek 2019.
#' Interpretable Segmentation of Medical Free-Text
#' Records Based on Word Embeddings. arXiv preprint arXiv:1907.04152.
"terms_pairs_test"
