\name{aoristic.shp}
\alias{aoristic.shp}
\title{aoristic graph by shapefile boundary}
\usage{
aoristic.shp(spdf, area.shp)
}
\arguments{
  \item{spdf}{spatial point data frame produced from
  aoristic.spdf}

  \item{area.shp}{spatial polygon data frame used as a
  boundary in WGS84}
}
\value{
kml file (an output folder will be generated in the current
working directory)
}
\description{
aoristic graph by shapefile boundary
}
\examples{
\donttest{
data(aoristic)
data.spdf <- aoristic.spdf(data=arlington,
   DateTimeFrom="DateTimeFrom", DateTimeTo="DateTimeTo",
   lon="lon", lat="lat")
aoristic.shp(spdf=data.spdf, area.shp=CouncilDistrict)
}
}
\references{
Ratcliffe, J. H. (2002). Aoristic Signatures and the
Spatio-Temporal Analysis of High Volume Crime Patterns.
Journal of Quantitative Criminology, 18(1), 23-43.
}

