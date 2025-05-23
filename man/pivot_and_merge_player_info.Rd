% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{pivot_and_merge_player_info}
\alias{pivot_and_merge_player_info}
\title{Pivot players to columns}
\usage{
pivot_and_merge_player_info(dkParsed, playerResults, salaryDf)
}
\arguments{
\item{dkParsed}{Draftkings Dataframe of entry information containing players}

\item{playerResults}{Draftkings Dataframe of MLB player results}

\item{salaryDf}{Draftkings Dataframe of player salaries for the slate}
}
\value{
none
}
\description{
Pivot players to columns
}
\examples{
\dontrun{
long_lineup_df = pivot_and_merge_player_info(dk_parsed, player_results, playerSalaryDataset)
}

}
