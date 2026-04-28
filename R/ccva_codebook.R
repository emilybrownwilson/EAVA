#' CCVA codebook: mapping from 2016 WHO Questionnaire to openVA variable format
#'
#' This dataset provides a transparent link between the original WHO VA
#' instrument and the OpenVA-compatible format produced by
#' \code{odk2EAVA()}.
#'
#' Each row represents a mapping from a WHO 2016 variable to an OpenVA
#' indicator, along with a human-readable description of the questionnaire item.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{who2016_var}{WHO 2016 variable identifier (e.g., \code{Id10147})}
#'   \item{question}{WHO 2016 verbal autopsy question text}
#'   \item{openVA_var}{Derived OpenVA variable name (e.g., \code{i147o})}
#' }
#'
#' @details
#' The OpenVA variables are binary indicators derived from responses to the
#' WHO 2016 Verbal Autopsy questionnaire and are used as inputs to
#' InterVA/OpenVA cause-of-death models.
#'
#' This dataset enables users to:
#' \itemize{
#'   \item interpret OpenVA variable names
#'   \item trace variables back to the original WHO questionnaire
#'   \item verify correctness of data transformations
#' }
#'
#' @source
#' WHO 2016 Verbal Autopsy Instrument
#' https://www.who.int/publications/m/item/verbal-autopsy-standards-the-2016-who-verbal-autopsy-instrument
#' InterVA-5 version 5.1 user guide:
#' \url{http://www.byass.uk/interva/products.htm}
#'
#' @examples
#' data(ccva_codebook)
#'
#' # View first few mappings
#' head(ccva_codebook)
#'
#' # Look up a specific OpenVA variable
#' subset(ccva_codebook, openVA_var == "i147o")
#'
#' # Trace a WHO variable to OpenVA variables
#' subset(ccva_codebook, who2016_var == "Id10147")
"ccva_codebook"
