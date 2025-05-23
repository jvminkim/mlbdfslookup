% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{extract_between}
\alias{extract_between}
\title{Extract player name from DK lineups}
\usage{
extract_between(string, start, stop)
}
\arguments{
\item{string}{Lineup cell to be extracted}

\item{start}{Which position the name plays}

\item{stop}{Which position to stop at}
}
\value{
A dataframe with player columns added
}
\description{
Extract player name from DK lineups
}
\examples{
\dontrun{
mutate(dk_tournament, 1b = extract_between(Lineup, "1B", "2B"))
}
}
