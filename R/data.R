#' Example 2016 WHO Verbal Autopsy Dataset from the Countrywide Mortality Surveillance for Action (COMSA) project in Mozambique
#'
#' This dataset contains a subset of individual-level responses to the 2016 WHO Verbal Autopsy Instrument as implemented in the
#' Countrywide Mortality Surveillance for Action (COMSA) project in Mozambique.
#' @format `data_public` is an example data frame with 10 rows and 511 columns comprising verbal autopsy questionnaire modules
#' \describe{
#'   \item{IDs, geolocation, weighting}{comsa_id-adj_wt2007}
#'   \item{indetifiers of respondent and interviewer}{Id10007-Id10011}
#'   \item{age-related variables}{All variables with names beginning "age" or "is", such as ageInDays or isNeonatal1}
#'   \item{information on the deceased}{Id10017-Id10024, Id10051-Id10066}
#'   \item{documentation of civil registration}{Id10069-Id10073}
#'   \item{verification of possible stillbirth}{Id10104-ID10116}
#'   \item{history of injuries and accidents}{Id10077-Id10100}
#'
#'   \item{medical history associated with final illness}{Id10123-Id10144}
#'   \item{general signs and symptoms associated with final illness}{Id10147-Id10159}
#'   \item{breathing difficulty}{Id10161-Id10176}
#'   \item{chest pain}{Id10178-Id10179}
#'   \item{loose stools, vomiting, abdominal pain}{Id10181-Id10205}
#'   \item{stiff neck, mental confusion, unconciousness, convulsions, skin conditions}{Id10207-ID10242}
#'   \item{wasting, swelling, lumps, paralysis, discoloration, water consumption}{Id10243-Id10270}
#'   \item{infant symptom reporting on deaths in children under 1 year}{Id10271-Id10283}
#'   \item{neonatal symptom reporting on deaths in children under 28 days}{Id10284-Id10290, Id10352-Id10357}
#'   \item{infant symptom reporting on deaths in children under 1 year}{Id10271-Id10283}
#'   \item{signs and symptoms associated with pregnancy}{Id10294-Id10310}
#'   \item{questions about possible maternal symptoms and delivery}{Id10312-Id10339, Id10340-Id10347, Id10358-Id10394, Id10376-Id10406}
#'   \item{alcohol and tobacco use}{Id10411-Id10416}
#'   \item{health service utilization}{Id10418-Id10446}
#'   \item{background and contexg}{Id10450-Id10459}
#'   \item{Death certificate with cause of death}{Id10462-Id10473}
#'   \item{Open narrative}{Id10476-Id10481}
#' }
#' @source data_public is a subset of the publicly available data from the Countrywide Mortality Surveillance for Action (COMSA) project: https://comsamozambique.org/data-access
#' @source the data capture system for this mortality survey was adapted from the 2016 WHO verbal autopsy instrument: https://www.who.int/publications/m/item/verbal-autopsy-standards-the-2016-who-verbal-autopsy-instrument
"data_public"
