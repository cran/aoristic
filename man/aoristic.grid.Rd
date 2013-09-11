\name{aoristic.grid}
\alias{aoristic.grid}
\title{aoristic graph by grid count}
\usage{
  aoristic.grid(spdf)
}
\arguments{
  \item{spdf}{spatial point data frame produced from
  aoristic.spdf}
}
\value{
  kml file (an output folder will be generated in the
  current working directory)
}
\description{
  aoristic graph by grid count
}
\examples{
\donttest{
data.spdf <- aoristic.spdf(data=arlington,
   DateTimeFrom="DateTimeFrom", DateTimeTo="DateTimeTo",
   lon="lon", lat="lat")
aoristic.grid(spdf=data.spdf)
}
}
\references{
  Ratcliffe, J. H. (2002). Aoristic Signatures and the
  Spatio-Temporal Analysis of High Volume Crime Patterns.
  Journal of Quantitative Criminology, 18(1), 23-43.
}

