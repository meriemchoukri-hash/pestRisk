\name{download_gbif}
\alias{download_gbif}
\title{Download pest occurrence records from GBIF}
\usage{
download_gbif(
  species,
  limit = 5000,
  country = NULL,
  year = NULL,
  basis_of_record = c("HUMAN_OBSERVATION", "OBSERVATION",
    "PRESERVED_SPECIMEN", "MACHINE_OBSERVATION"),
  verbose = TRUE
)
}
\arguments{
\item{species}{\code{character}. Full scientific name of the pest, e.g.
\code{"Spodoptera frugiperda"}, \code{"Tuta absoluta"}.}

\item{limit}{\code{integer}. Maximum number of records to request
(default 5000; GBIF hard cap is 100 000 per call).}

\item{country}{\code{character} or \code{NULL}. ISO 3166-1 alpha-2 code to
restrict the search geographically (e.g. \code{"MA"} for Morocco).
\code{NULL} performs a global search (default).}

\item{year}{\code{character} or \code{NULL}. Year range as
\code{"YYYY,YYYY"}, e.g. \code{"2010,2023"}. \code{NULL} keeps all years.}

\item{basis_of_record}{\code{character vector}. Allowed observation types.
Default keeps human observations and specimen-based records.}

\item{verbose}{\code{logical}. Print download progress (default \code{TRUE}).}
}
\value{
A \code{data.frame} containing at minimum the columns:
\code{species}, \code{decimalLongitude}, \code{decimalLatitude},
\code{year}, \code{country}, \code{gbifID},
\code{coordinateUncertaintyInMeters}, \code{basisOfRecord}.
Returns an empty \code{data.frame} if no records are found.
}
\description{
Downloads and pre-filters occurrence data for an agricultural pest species
from the Global Biodiversity Information Facility (GBIF). Records without
coordinates, with geospatial issues, or exact duplicates are automatically
removed.
}
\details{
The function calls \code{rgbif::occ_search()} with
\code{hasCoordinate = TRUE} and \code{hasGeospatialIssue = FALSE}.

Post-download filters applied:
\enumerate{
  \item Records with \code{NA} coordinates are removed.
  \item Coordinates outside valid ranges are removed.
  \item The "Null Island" artefact (0°, 0°) is removed.
  \item Exact duplicates on (longitude, latitude, year) are removed.
}
}
\examples{
\dontrun{
# Download fall armyworm occurrences globally
occ <- download_gbif("Spodoptera frugiperda", limit = 500)
head(occ)

# Restrict to Morocco, last 10 years
occ_ma <- download_gbif("Tuta absoluta",
                         country = "MA",
                         year    = "2013,2023",
                         limit   = 300)
}
}
\references{
GBIF Secretariat (2023). GBIF Backbone Taxonomy.
\url{https://doi.org/10.15468/39omei}
}
