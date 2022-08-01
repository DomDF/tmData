#' get_TM_squad
#'
#' A webscraping function for the website 'https://www.transfermarkt.com/'.
#' Returns a preprocessed tibble of any squad.
#'
#'
#' @param squad_url A valid ()detailed) transfermarkt url. Navigate to a club page, click on the 'detailed' tab, and then paste the url - REQUIRED.
#' @param user_agent A character introducing yourself to the website - REQUIRED.
#' @param raw  Logical: if TRUE, returns the unprocessed table. if FALSE, returns a processed tibble - defaults to FALSE
#' @param year Numeric: specify the year for which you would like the data
#' @keywords Webscraping, Football, Soccer
#' @export
#' @examples
#' Juventus squad <- get_TM_squad(squad_url = 'https://www.transfermarkt.co.uk/juventus-turin/startseite/verein/506', user_agent = 'John Smith personal project')

get_TM_squad <- function(squad_url, user_agent, raw = FALSE, year = 2022){

  require(tidyverse); require(polite)

  url <- stringr::str_replace(string = squad_url, pattern = 'startseite', replacement = 'kader') |>
    paste0('/saison_id/') |>
    paste0(year) |>
    paste0('/plus/1')

  session <- polite::bow(url = url, user_agent = user_agent)
  ws <- session |>
    polite::scrape()

  raw_squad <- ws |>
    rvest::html_nodes('#yw1 > table') |>
    rvest::html_table(fill = TRUE)

  team_name <- ws |>
    rvest::html_nodes('#verein_head > div > div.dataHeader.dataExtended > div.dataMain > div > div.dataName > h1') |>
    rvest::html_text() |>
    stringr::str_replace_all(pattern = '\n', replacement = '') |>
    stringr::str_trim()

  page_urls <- ws |>
    rvest::html_nodes('#yw1 > table') |>
    rvest::html_nodes('td') |>
    rvest::html_nodes(css = 'a') |>
    rvest::html_attr('href')

  player_urls <- page_urls[grepl(pattern = '/spieler/', x = page_urls)]

  squad <- raw_squad |>
    as.data.frame() |> tibble::as_tibble() |>
    dplyr::mutate(X. = X. |> as.numeric()) |>
    dplyr::filter(!is.na(Nat.))

  if('Current.club' %in% colnames(squad)){
    cols <- c('number', NA, NA, 'player', 'position', 'DoB', NA, NA, 'height_m', 'foot', 'joined', NA, 'contract_exp', 'market_value')
    colnames(squad) <- cols; squad <- squad[!is.na(names(squad))]
  } else {
    cols <- c('number', NA, NA, 'player', 'position', 'DoB', NA, 'height_m', 'foot', 'joined', NA, 'contract_exp', 'market_value')
    colnames(squad) <- cols; squad <- squad[!is.na(names(squad))]
  }

  squad <- squad |>
    dplyr::mutate(currency = dplyr::case_when(
      grepl(pattern = '£', x = market_value) ~ '£',
      grepl(pattern = '€', x = market_value) ~ '€',
      grepl(pattern = '$', x = market_value) ~ '$'
    )) |>
    dplyr::mutate(market_value = sub(pattern = '£|€|$', replacement = '', x = market_value)) |>
    dplyr::mutate(market_value = dplyr::case_when(
      grepl(pattern = 'm', x = market_value) ~ 1e6 * as.numeric(sub(pattern = 'm',
                                                                        replacement = '',
                                                                        x = market_value)),
      grepl(pattern = 'Th.', x = market_value) ~ 1e3 * as.numeric(sub(pattern = 'Th.',
                                                                          replacement = '',
                                                                          x = market_value))
    )) |>
    dplyr::mutate(contract_exp = lubridate::mdy(gsub(pattern = ' ', replacement = '-',
                                                     x = gsub(pattern = ',', replacement = '', x = contract_exp)))) |>
    dplyr::mutate(joined = lubridate::mdy(gsub(pattern = ' ', replacement = '-',
                                               x = gsub(pattern = ',', replacement = '', x = joined)))) |>
    dplyr::mutate(height_m = as.numeric(gsub(pattern = ',', replacement = '.',
                                             x = gsub(pattern = ' m', replacement = '', x = height_m)))) |>
    dplyr::mutate(DoB = lubridate::mdy(gsub(pattern = ' ', replacement = '-',
                                     x = gsub(pattern = ',', replacement = '',
                                              x = stringr::str_replace(string = stringr::str_sub(DoB, end = -5),
                                                                       pattern = " \\(.*",
                                                                       replacement = ''))))) |>
    dplyr::mutate(surname = stringr::str_trim(string = stringr::word(string =  player, sep = '[ ]', start = -1),
                                              side = 'both'),
                  name = stringr::word(string = player, sep = ' ', end = 1)) |>
    dplyr::mutate(age = lubridate::interval(start = DoB, end = Sys.Date()) / lubridate::years(1)) |>
    dplyr::mutate(squad_value = sum(market_value)) |>
    dplyr::mutate(year = year,
                  player = forcats::fct_reorder(.f = as.factor(player), .x = market_value),
                  position = forcats::as_factor(position),
                  foot = forcats::as_factor(foot),
                  team = team_name,
                  player_url = purrr::map_chr(.x = player_urls,
                                              .f = function(.x){paste0('https://www.transfermarkt.co.uk', .x)[[1]]})) |>
    dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) |>
    dplyr::arrange(desc(market_value))

  # squad$player_name = character(length = squad |> nrow())
  #
  # for (i in seq(from = 1, to = nrow(squad))){
  #
  #   if(grepl(pattern = squad$name[i], x = squad$surname[i])) {
  #
  #     squad$player_name[i] <- stringr::str_sub(string = squad$surname[i], start = 1,
  #                                              end = nchar(squad$surname[i])/2)
  #
  #   } else {
  #     squad$player_name[i] <- paste(squad$name[i], squad$surname[i])
  #   }
  #
  # }

  if(raw == FALSE){
    return(squad)
  } else {
    return(raw_squad)
  }

}



