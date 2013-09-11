\name{aoristic.all.graph}
\alias{aoristic.all.graph}
\title{Creating a general aoristic graph using all data}
\usage{
  aoristic.all.graph(data)
}
\arguments{
  \item{data}{data.frame created by aoristic.df}
}
\value{
  graph data.frame with two columns (hour, frequency) used
  to create an aoristic graph for the entire study area
}
\description{
  Creating a general aoristic graph using all data
}
\examples{
\donttest{
data <- aoristic.df(data=arlington)
graph <- aoristic.all.graph(data=data)
ggplot(graph, aes(x=hour, y=freq)) +
   geom_bar(stat="identity") +
   ggtitle("Aoristic Graph for the Entire Study Area")
}
}

