\name{aoristic.spdf}
\alias{aoristic.spdf}
\title{creating a spatial.polygon.data.frame for aoristic analysis}
\usage{
aoristic.spdf(data, DateTimeFrom, DateTimeTo, lon, lat)
}
\arguments{
  \item{data}{data.frame with a minimum of 4 columns
  representing FromDateTime, ToDateTime, lon, lat}

  \item{DateTimeFrom}{a character vector of the column name
  for FromDateTime (POSIXct date-time object)}

  \item{DateTimeTo}{a character vector of the column name
  for ToDateTime (POSIXct date-time object).  If ending
  date-time is missing, the duration of an event will be
  coded as 1 hour.}

  \item{lon}{a character vector longitude}

  \item{lat}{a character vector of the column name for
  latitude}
}
\value{
spatial point data frame (SPDF)
}
\description{
creating a spatial.polygon.data.frame for aoristic analysis
}
\examples{
\donttest{
data(aoristic)
data.spdf <- aoristic.spdf(data=arlington,
   DateTimeFrom="FrmDateTimeFull", DateTimeTo="ToDateTimeFull",
   lon="lon", lat="lat")
}
}
\references{
Ratcliffe, J. H. (2002). Aoristic Signatures and the
Spatio-Temporal Analysis of High Volume Crime Patterns.
Journal of Quantitative Criminology, 18(1), 23-43.
}

