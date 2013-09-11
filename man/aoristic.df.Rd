\name{aoristic.df}
\alias{aoristic.df}
\title{creating a data.frame for aoristic analysis}
\usage{
  aoristic.df(data, DateTimeFrom, DateTimeTo)
}
\arguments{
  \item{data}{data.frame with a minimum of 2 columns
  representing FromDateTime, and ToDateTime}

  \item{DateTimeFrom}{a character vector of the column name
  for FromDateTime (POSIXct date-time object)}

  \item{DateTimeTo}{a character vector of the column name
  for ToDateTime (POSIXct date-time object).  If ending
  date-time is missing, duration of an event will be coded
  as 1 hour.}
}
\value{
  data.frame
}
\description{
  creating a data.frame for aoristic analysis
}
\examples{
\donttest{
data2 <- aoristic.df(data=arlington,
   DateTimeFrom="DateTimeFrom", DateTimeTo="DateTimeTo")
head(data2)
}
}
\references{
  Ratcliffe, J. H. (2002). Aoristic Signatures and the
  Spatio-Temporal Analysis of High Volume Crime Patterns.
  Journal of Quantitative Criminology, 18(1), 23-43.
}

