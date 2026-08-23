#' Example health survey dataset
#'
#' A realistic-but-fictional health survey dataset with 200 synthetic
#' records. Contains demographics, clinical measures, and haven-labelled
#' smoking status. No real patient data.
#'
#' @format A tibble with 200 rows and 10 columns:
#' \describe{
#'   \item{record_id}{Character. Record identifier.}
#'   \item{age}{Numeric. Age in years.}
#'   \item{sex}{Factor. Biological sex (Male / Female).}
#'   \item{bmi}{Numeric. Body mass index, with some missing values.}
#'   \item{smoking_status}{haven_labelled. Smoking status (Current / Former / Never).}
#'   \item{systolic_bp}{Numeric. Systolic blood pressure, some missing.}
#'   \item{diastolic_bp}{Numeric. Diastolic blood pressure.}
#'   \item{survey_date}{Date. Date of survey response.}
#'   \item{province}{Factor. Canadian province abbreviation.}
#'   \item{comments}{Character. Free-text comments, some missing.}
#' }
"example_health_survey"

#' Example administrative claims dataset
#'
#' A realistic-but-fictional administrative claims dataset with 300 synthetic
#' records. Contains claim identifiers, procedure codes (haven-labelled),
#' costs, and provider locations. No real patient data.
#'
#' @format A tibble with 300 rows and 9 columns:
#' \describe{
#'   \item{claim_id}{Integer. Claim identifier.}
#'   \item{patient_id}{Character. Patient identifier.}
#'   \item{service_date}{Date. Date of service.}
#'   \item{dx_code}{Factor. Diagnosis code.}
#'   \item{proc_code}{haven_labelled. Procedure type (Consult / Surgery / Lab / Imaging).}
#'   \item{cost}{Numeric. Claim cost in dollars, some missing.}
#'   \item{approved}{Logical. Whether the claim was approved, some missing.}
#'   \item{provider_city}{Character. City of the service provider.}
#'   \item{postal_code}{Character. Forward sortation area (FSA).}
#' }
"example_admin_claims"

#' Example disease registry dataset
#'
#' A realistic-but-fictional disease registry dataset with 150 synthetic
#' records. Contains enrollment data, disease staging (haven-labelled),
#' biomarker values, and patient status. No real patient data.
#'
#' @format A tibble with 150 rows and 10 columns:
#' \describe{
#'   \item{subject_id}{Character. Subject identifier.}
#'   \item{enroll_date}{Date. Date of enrollment.}
#'   \item{age_at_enroll}{Numeric. Age at enrollment in years.}
#'   \item{disease_stage}{haven_labelled. Disease stage (Stage I-IV).}
#'   \item{biomarker_a}{Numeric. Biomarker A level, some missing.}
#'   \item{biomarker_b}{Numeric. Biomarker B level, some missing.}
#'   \item{status}{Factor. Current patient status.}
#'   \item{last_visit}{Date. Date of last follow-up visit, some missing.}
#'   \item{region}{Factor. Geographic region.}
#'   \item{notes}{Character. Clinical notes, some missing.}
#' }
"example_registry"

#' Individual-level synthetic sample data
#'
#' A synthetically generated dataset of 200 individual records for use as
#' sample input in the DataGangeR Shiny app. Contains demographic and health
#' variables with realistic distributions. Generated with \code{set.seed(42)}.
#'
#' @format A data frame with 200 rows and 7 columns:
#' \describe{
#'   \item{id}{Integer record identifier}
#'   \item{age}{Age in years (18--85)}
#'   \item{sex}{Sex (Male / Female / Other)}
#'   \item{income}{Annual income in dollars (log-normal, some NAs)}
#'   \item{education}{Highest education level}
#'   \item{smoker}{Logical smoking status}
#'   \item{bmi}{Body mass index}
#' }
#' @source Synthetically generated via \code{data-raw/individual_sample.R}
"individual_sample"

#' Temporal synthetic sample data
#'
#' A synthetically generated dataset of 365 daily records for use as sample
#' input in the DataGangeR Shiny app. Simulates environmental monitoring
#' data across multiple sites. Generated with \code{set.seed(42)}.
#'
#' @format A data frame with 365 rows and 5 columns:
#' \describe{
#'   \item{date}{Measurement date (daily from 2023-01-01)}
#'   \item{site_id}{Site identifier (SITE_A through SITE_E)}
#'   \item{measurement}{Numeric measurement value (some NAs)}
#'   \item{temperature}{Ambient temperature in degrees Celsius}
#'   \item{flagged}{Logical quality-control flag}
#' }
#' @source Synthetically generated via \code{data-raw/temporal_sample.R}
"temporal_sample"

#' Geographic synthetic sample data
#'
#' A synthetically generated dataset of 50 regional summary records for use
#' as sample input in the DataGangeR Shiny app. Simulates public-health
#' surveillance data aggregated by region. Generated with \code{set.seed(42)}.
#'
#' @format A data frame with 50 rows and 5 columns:
#' \describe{
#'   \item{region}{Region identifier (Region_01 through Region_50)}
#'   \item{population}{Regional population count}
#'   \item{rate_per_100k}{Event rate per 100,000 population}
#'   \item{category}{Area classification (Urban / Suburban / Rural)}
#'   \item{risk_level}{Assigned risk level (Low / Medium / High)}
#' }
#' @source Synthetically generated via \code{data-raw/Geographic_sample.R}
"geographic_sample"
