#' get_TM_player
#'
#' A webscraping function for the website 'https://www.transfermarkt.com/'.
#' Returns a preprocessed tibble of a player.
#'
#'
#' @param player_url A valid transfermarkt url. Navigate to a player's page, and then copy the url - REQUIRED.
#' @param user_agent A character introducing yourself to the website - REQUIRED.
#' @param raw  Logical: if TRUE, returns the unprocessed table. if FALSE, returns a processed tibble - defaults to FALSE
#' @keywords Webscraping, Football, Soccer
#' @export
#' @examples
#' dusan_vlahovic <- get_TM_player(player_url = 'https://www.transfermarkt.co.uk/du-scaron-an-vlahović/profil/spieler/357498', user_agent = 'John Smith personal project')

get_TM_player <- function(player_url, user_agent, raw = FALSE){

  require(tidyverse); require(polite)

  url <- player_url |> gsub(pattern = 'profil', replacement = 'leistungsdatendetails') |>
    paste0('/saison//verein/0/liga/0/wettbewerb//pos/0/trainer_id/0/plus/1')

  session <- polite::bow(url = url, user_agent = user_agent)

  raw_player <- session |>
    polite::scrape() |>
    rvest::html_nodes('#yw1 > table') |>
    rvest::html_table(fill = TRUE)

  page_urls <- session |>
    polite::scrape() |>
    rvest::html_nodes('#yw1 > table') |>
    rvest::html_nodes('td') |>
    rvest::html_nodes(css = 'a') |>
    rvest::html_attr('href')

  club_urls <- page_urls[grepl(pattern = '/startseite/verein/', x = page_urls)]

  get_team_name <- function(club_url){

    polite::bow(url = club_url, user_agent = user_agent) |>
      polite::scrape() |>
      rvest::html_nodes('#verein_head > div > div.dataHeader.dataExtended > div.dataMain > div > div.dataName > h1') |>
      rvest::html_text() |>
      stringr::str_replace_all(pattern = '\n', replacement = '') |>
      stringr::str_trim()

  }

  clubs <- purrr::map_chr(.x = club_urls,
                          .f = function(.x) {get_team_name(club_url = paste0('https://www.transfermarkt.co.uk', .x))})

  cols <- c('season', NA, 'competition', 'team', NA, 'apps', 'points_per_game', 'goals', 'assists', 'own_goals',
            'subbed_on', 'subbed_off', 'yellow_cards', 'second_yellows', 'straight_reds', NA, NA, 'mins_played')

  player <- raw_player |>
    as.data.frame() |> as_tibble() |>
    dplyr::filter(Season != "") |>
    dplyr::mutate(dplyr::across(.cols = everything(),
                                .fns = ~ stringr::str_replace_all(string = ., pattern = '-', replacement = '0')))

  colnames(player) <- cols; player <- player[!is.na(names(player))]

  player <- player |>
    dplyr::mutate(team = clubs,
                  season = forcats::as_factor(x = season),
                  competition = forcats::as_factor(x = competition),
                  team = forcats::as_factor(x = team),
                  apps = as.integer(x = apps),
                  points_per_game = as.double(x = points_per_game),
                  goals = as.integer(x = goals),
                  assists = as.integer(x = assists),
                  own_goals = as.integer(x = own_goals),
                  subbed_on = as.integer(x = subbed_on),
                  subbed_off = as.integer(x = subbed_off),
                  yellow_cards = as.integer(x = yellow_cards),
                  second_yellows = as.integer(x = second_yellows),
                  straight_reds = as.integer(x = straight_reds),
                  mins_played = stringr::str_remove(string = mins_played, pattern = "'"),
                  mins_played = as.double(x = mins_played),
                  mins_played = dplyr::case_when(
                    mins_played %% 1 != 0 ~ mins_played * 1e3,
                    T ~ mins_played
                  ),
                  red_cards = second_yellows + straight_reds

    )

  if(raw == FALSE){
    return(player)
  } else {
    return(raw_player)
  }

}
