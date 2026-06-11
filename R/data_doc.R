# =============================================================================
# data_doc.R  –  Documentation of built-in example datasets
# =============================================================================

#' Simulated occurrence records for Spodoptera frugiperda
#'
#' @description
#' A simulated dataset of 200 georeferenced occurrence records for the fall
#' armyworm (*Spodoptera frugiperda*), representative of the species' known
#' distribution across sub-Saharan Africa and the Americas.
#' Generated for package demonstration purposes; coordinates follow the
#' real biogeographic range of the species.
#'
#' @format A `data.frame` with 200 rows and 8 columns:
#' \describe{
#'   \item{`species`}{Scientific name: `"Spodoptera frugiperda"`}
#'   \item{`decimalLongitude`}{Longitude in decimal degrees (WGS84)}
#'   \item{`decimalLatitude`}{Latitude in decimal degrees (WGS84)}
#'   \item{`year`}{Year of observation (2010–2023)}
#'   \item{`country`}{ISO 3166-1 alpha-2 country code}
#'   \item{`gbifID`}{Simulated GBIF record identifier}
#'   \item{`coordinateUncertaintyInMeters`}{Positional uncertainty in metres}
#'   \item{`basisOfRecord`}{Always `"HUMAN_OBSERVATION"`}
#' }
#'
#' @details
#' The fall armyworm is a highly polyphagous pest native to the Americas that
#' invaded Africa in 2016. It attacks over 80 plant species, with maize
#' (*Zea mays*) being the primary host. Its distribution is limited by cold
#' winter temperatures (BIO6 < 10°C kills overwintering populations).
#'
#' Data can be loaded directly:
#' ```r
#' spodoptera_occ <- read.csv(
#'   system.file("extdata", "spodoptera_occ.csv", package = "pestRisk")
#' )
#' ```
#'
#' @source Simulated from known GBIF distribution data.
#'   Real data available at: <https://www.gbif.org/species/9240015>
#'
#' @references
#' Early, R. *et al.* (2018). Global threats from invasive alien species in the
#' twenty-first century and national response capacities. *Nature Communications*
#' **9**, 1–9. <doi:10.1038/s41467-018-02965-0>
#'
#' @examples
#' path <- system.file("extdata", "spodoptera_occ.csv", package = "pestRisk")
#' spodoptera_occ <- read.csv(path)
#' head(spodoptera_occ)
#' table(spodoptera_occ$country)
"spodoptera_occ"


#' Simulated occurrence records for Tuta absoluta
#'
#' @description
#' A simulated dataset of 150 georeferenced occurrence records for the tomato
#' leafminer (*Tuta absoluta*), covering the Mediterranean basin (including
#' Morocco, Spain, Italy, Turkey) and South America, reflecting the species'
#' invasive spread from its Andean origin since 2006.
#'
#' @format A `data.frame` with 150 rows and 8 columns:
#' \describe{
#'   \item{`species`}{Scientific name: `"Tuta absoluta"`}
#'   \item{`decimalLongitude`}{Longitude in decimal degrees (WGS84)}
#'   \item{`decimalLatitude`}{Latitude in decimal degrees (WGS84)}
#'   \item{`year`}{Year of observation (2008–2023)}
#'   \item{`country`}{ISO 3166-1 alpha-2 country code}
#'   \item{`gbifID`}{Simulated GBIF record identifier}
#'   \item{`coordinateUncertaintyInMeters`}{Positional uncertainty in metres}
#'   \item{`basisOfRecord`}{Always `"HUMAN_OBSERVATION"`}
#' }
#'
#' @details
#' *Tuta absoluta* is one of the most damaging pests of tomato worldwide,
#' causing 80–100% yield losses in unmanaged crops. It is particularly relevant
#' for Morocco, where tomato is a major export crop.
#'
#' ```r
#' tuta_occ <- read.csv(
#'   system.file("extdata", "tuta_occ.csv", package = "pestRisk")
#' )
#' ```
#'
#' @source Simulated from known GBIF distribution data.
#'   Real data available at: <https://www.gbif.org/species/1892170>
#'
#' @examples
#' path <- system.file("extdata", "tuta_occ.csv", package = "pestRisk")
#' tuta_occ <- read.csv(path)
#' hist(tuta_occ$decimalLatitude, main = "Latitude distribution – T. absoluta")
"tuta_occ"


#' Pre-built modelling dataset for Spodoptera frugiperda
#'
#' @description
#' A ready-to-use modelling table combining simulated presence records and
#' background points for *Spodoptera frugiperda*, with extracted bioclimatic
#' predictor values. This dataset is used directly by [train_rf_model()] and
#' [train_maxent()] without requiring internet access or raster downloads.
#'
#' @format A `data.frame` with 650 rows and 6 columns:
#' \describe{
#'   \item{`lon`}{Longitude in decimal degrees}
#'   \item{`lat`}{Latitude in decimal degrees}
#'   \item{`presence`}{`1` = presence record, `0` = background point}
#'   \item{`BIO1`}{Annual Mean Temperature (°C × 10)}
#'   \item{`BIO12`}{Annual Precipitation (mm)}
#'   \item{`BIO15`}{Precipitation Seasonality (coefficient of variation)}
#' }
#'
#' @details
#' 150 presence records + 500 background points. The 3 bioclimatic variables
#' were selected after VIF and Pearson filtering from the full BIO1–19 set.
#' BIO values follow WorldClim v2.1 units and scaling.
#'
#' ```r
#' model_data <- read.csv(
#'   system.file("extdata", "spodoptera_model_data.csv", package = "pestRisk")
#' )
#' ```
#'
#' @examples
#' path <- system.file("extdata", "spodoptera_model_data.csv",
#'                      package = "pestRisk")
#' model_data <- read.csv(path)
#' table(model_data$presence)
#' rf_out <- train_rf_model(model_data, n_trees = 100)
"spodoptera_model_data"
