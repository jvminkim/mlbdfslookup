library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)


#' Extract player name from DK lineups
#'
#' @param string Lineup cell to be extracted
#' @param start Which position the name plays
#' @param stop Which position to stop at
#'
#' @return A dataframe with player columns added
#' @export
#'
#' @examples
#' \dontrun{
#' mutate(dk_tournament, 1b = extract_between(Lineup, "1B", "2B"))
#' }

extract_between = function(string, start, stop) {
  pattern = paste0("\\b", start, "\\b (.+?)(?= \\b", stop, "\\b)")
  matches = stringr::str_match(string, pattern)

  # Return just the second column (the captured group), with NA protection
  if (is.null(dim(matches))) {
    return(rep(NA_character_, length(string)))
  }

  return(matches[, 2])
}

#' Add indexing to positions where there are multiple, i.e. OF and P
#'
#' @param string Lineup cell to be extracted
#' @param tag Specific position to index
#'
#' @return A dataframe with indexed positions added to the Lineup
#' @export
#'
#' @examples
#' \dontrun{
#' mutate(Lineup = sapply(Lineup, function(l) {
#'  l %>%
#'   replace_with_index("OF") %>%
#'   replace_with_index("P")
#' }))
#' }
#'
#'

replace_with_index = function(string, tag) {
  matches = stringr::str_locate_all(string, paste0("\\b", tag, "\\b"))[[1]]
  if (nrow(matches) == 0) return(string)

  for (i in seq_len(nrow(matches))) {
    string = sub(paste0("\\b", tag, "\\b"), paste0(tag, i), string)
  }
  string
}

#' Parse through dataframe to extract player names from lineups
#'
#' @param entryDf Draftkings Dataframe of entry information containing lineups
#'
#' @return none
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dk_parsed = parse_lineup_positions(entry_df)
#' }
#'

parse_lineup_positions = function(entryDf) {
  entryDf |>
    dplyr::mutate(Lineup = sapply(Lineup, function(l) {
      l |> replace_with_index("OF") |> replace_with_index("P")
    })) |>
    dplyr::mutate(
      `1B` = extract_between(Lineup, "1B", "2B"),
      `2B` = extract_between(Lineup, "2B", "3B"),
      `3B` = extract_between(Lineup, "3B", "C"),
      C    = extract_between(Lineup, "C", "OF1"),
      OF1  = extract_between(Lineup, "OF1", "OF2"),
      OF2  = extract_between(Lineup, "OF2", "OF3"),
      OF3  = extract_between(Lineup, "OF3", "P1"),
      P1   = extract_between(Lineup, "P1", "P2"),
      P2   = extract_between(Lineup, "P2", "SS"),
      SS   = stringr::str_extract(Lineup, "(?<=SS )(.+)")
    ) |>
    dplyr::select(-Lineup)
}

#' Pivot players to columns
#'
#' @param dkParsed Draftkings Dataframe of entry information containing players
#' @param playerResults Draftkings Dataframe of MLB player results
#' @param salaryDf Draftkings Dataframe of player salaries for the slate
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' long_lineup_df = pivot_and_merge_player_info(dk_parsed, player_results, playerSalaryDataset)
#' }
#'

pivot_and_merge_player_info = function(dkParsed, playerResults, salaryDf) {
  dkParsed |>
    tidyr::pivot_longer(
      cols = c(`1B`, `2B`, `3B`, C, OF1, OF2, OF3, P1, P2, SS),
      names_to = "Position",
      values_to = "Player"
    ) |>
    dplyr::left_join(playerResults, by = "Player") |>
    dplyr::rename(Drafted = `%Drafted`) |>
    dplyr::left_join(
      salaryDf |>
        dplyr::mutate(Name = stringr::str_trim(Name)) |>
        dplyr::distinct(Name, .keep_all = TRUE) |>
        dplyr::select(Player = Name, Salary, TeamAbbrev),
      by = "Player"
    ) |>
    dplyr::mutate(
      Position_fpts = paste0(Position, "_fpts"),
      Position_owner = paste0(Position, "owner"),
      Position_salary = paste0(Position, "_salary"),
      Position_team = paste0(Position, "_team")
    )
}

#' Widen lineup to add fantasy points, salary and team
#'
#' @param longLineupDf Dataframe of lineup with player information in a long format
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' wide_lineup = widen_lineup(longLineupDf)
#' }


widen_lineup = function(longLineupDf) {
  longLineupDf |>
    dplyr::select(
      EntryId, EntryName, TimeRemaining, Points,
      Position, Player, Position_fpts, FPTS,
      Position_owner, Drafted,
      Position_salary, Salary,
      Position_team, TeamAbbrev
    ) |>
    tidyr::pivot_wider(
      id_cols = c(EntryId, EntryName, TimeRemaining, Points),
      names_from = c(Position, Position_fpts, Position_owner, Position_salary, Position_team),
      values_from = c(Player, FPTS, Drafted, Salary, TeamAbbrev),
      names_glue = "{Position}{.value}",
      values_fn = list
    )
}

#' Summarize team stacks
#'
#' @param dkCombined DK Dataframe of all players and info added
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_team_stacks(wide_lineup)
#' }

summarize_team_stacks = function(dkCombined) {
  non_pitcher_positions = c("1B", "2B", "3B", "C", "OF1", "OF2", "OF3", "SS")
  team_columns = paste0(non_pitcher_positions, "TeamAbbrev")

  dkCombined |>
    dplyr::rowwise() |>
    dplyr::mutate(
      TeamStack = list(na.omit(c_across(dplyr::all_of(team_columns)))),
      StackCounts = list(sort(table(TeamStack), decreasing = TRUE)),
      StackSummary = if (length(StackCounts) > 0) {
        paste(paste(unlist(StackCounts), names(StackCounts)), collapse = " | ")
      } else {
        NA_character_
      },
      PrimaryStack = if (length(StackCounts) > 0 && max(StackCounts) >= 4) names(StackCounts)[1] else "None"
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-TeamStack, -StackCounts)
}


#' Read CSV and output players and stacks
#'
#' @param dkDataset Dataset of Draftkings tournament result
#'
#' @return A dataframe with players and stacks used
#' @export
#'
#' @examples
#' \dontrun{
#' mlbdfslookup::get_lineups(dk_4_14, dk_4_14_salary, "DK")
#' }
#'

get_lineups = function(dkDataset, playerSalaryDataset, salaryType) {
  player_results = dkDataset |>
    dplyr::select(Player, `Roster Position`, FPTS, `%Drafted`) |>
    dplyr::filter(!is.na(FPTS))

  entry_df = dkDataset |>
    dplyr::select(Rank, EntryId, EntryName, TimeRemaining, Points, Lineup)

  dk_parsed = parse_lineup_positions(entry_df)

  if (salaryType == "SS") {
    playerSalaryDataset = playerSalaryDataset |>
      dplyr::rename(TeamAbbrev = Team)
  }

  long_lineup_df = pivot_and_merge_player_info(dk_parsed, player_results, playerSalaryDataset)

  wide_lineup = widen_lineup(long_lineup_df)

  summarize_team_stacks(wide_lineup)
}

