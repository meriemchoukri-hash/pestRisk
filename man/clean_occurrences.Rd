\name{clean_occurrences}
\alias{clean_occurrences}
\title{Clean and spatially filter pest occurrence records}
\usage{
clean_occurrences(
  occ,
  lon_col = "decimalLongitude",
  lat_col = "decimalLatitude",
  thin_dist = 10,
  remove_sea = TRUE,
  outlier_sd = 3,
  verbose = TRUE
)
}
\arguments{
\item{occ}{\code{data.frame}. Raw occurrence records, typically the output
of \code{\link{download_gbif}}.}

\item{lon_col}{\code{character}. Name of the longitude column
(default \code{"decimalLongitude"}).}

\item{lat_col}{\code{character}. Name of the latitude column
(default \code{"decimalLatitude"}).}

\item{thin_dist}{\code{numeric}. Minimum distance in kilometres between any
two retained points for spatial rarefaction (default \code{10}).
Set to \code{0} to skip.}

\item{remove_sea}{\code{logical}. Remove points falling in the ocean using
world administrative boundaries (default \code{TRUE}).}

\item{outlier_sd}{\code{numeric} or \code{NULL}. Remove points whose
Mahalanobis distance from the species' centroid exceeds this many
standard deviations (default \code{3}).}

\item{verbose}{\code{logical}. Print the cleaning report table
(default \code{TRUE}).}
}
\value{
A named \code{list} with:
\describe{
  \item{\code{data}}{An \code{sf} object (CRS EPSG:4326) of cleaned occurrences.}
  \item{\code{report}}{A \code{data.frame} summarising records removed at each step.}
}
}
\description{
Applies a rigorous, multi-step spatial cleaning pipeline to raw occurrence
data. Steps include: removal of null/impossible coordinates, deduplication,
optional removal of marine/ocean points, outlier detection based on
Mahalanobis distance, and spatial thinning (rarefaction) to reduce
sampling bias.
}
\details{
Cleaning steps applied in order:
\tabular{ll}{
  1 \tab Remove NA coordinates \cr
  2 \tab Remove impossible and Null Island coordinates \cr
  3 \tab Remove spatial duplicates (4 decimal places ≈ 11 m) \cr
  4 \tab Remove marine / sea points \cr
  5 \tab Geographic outlier detection (Mahalanobis distance) \cr
  6 \tab Spatial thinning at \code{thin_dist} km \cr
}

Spatial thinning (step 6) is critical for SDMs: clustered occurrence
records (e.g., many observations from a single farm) bias the model toward
locally over-sampled environmental conditions.
}
\examples{
path <- system.file("extdata", "spodoptera_occ.csv", package = "pestRisk")
occ  <- read.csv(path)

result <- clean_occurrences(occ, thin_dist = 20, remove_sea = FALSE)
print(result$report)
plot(sf::st_geometry(result$data), pch = 20, col = "firebrick",
     main = "Cleaned S. frugiperda occurrences")
}
\references{
Aiello-Lammens et al. (2015) spThin: an R package for spatial thinning
of species occurrence records. \emph{Ecography} \strong{38}, 541-545.
\doi{10.1111/ecog.01132}
}
