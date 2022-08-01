library(tmData)

juve_df <- get_TM_squad(squad_url = 'https://www.transfermarkt.co.uk/juventus-turin/startseite/verein/506',
                        user_agent = 'Dom Di Francesco, personal project')

squad_value_plot <- function(years){

  plot_df <- tibble()
  for (year in years){
    plot_df <- rbind(plot_df,
                     get_TM_squad(squad_url = 'https://www.transfermarkt.co.uk/juventus-turin/startseite/verein/506',
                                  user_agent = 'Dom Di Francesco, personal project', year = year))
  }

  threshold = 50

  ggplot(data = plot_df, mapping = aes(x = age, y = market_value * 1e-6))+
    geom_point(shape = 1, alpha = 1/2, size = 1)+
    facet_wrap(facets = ~ year, ncol = 3)+
    ggrepel::geom_text_repel(data = plot_df |> dplyr::filter(market_value >= threshold * 1e6),
                             mapping = aes(x = age, y = market_value * 1e-6, label = player),
                             family = 'Atkinson Hyperlegible', size = 2, segment.alpha = 1/3)+
    geom_hline(yintercept = threshold, lty = 2, alpha = 1/4)+
    scale_x_continuous(name = 'Player age')+
    scale_y_continuous(name = 'Player value, £M')+
    ggthemes::theme_base(base_size = 9, base_family = 'Atkinson Hyperlegible') +
    theme(plot.background = element_rect(colour = NA))

}
squad_value_plot(years = seq(from = 2014, to = 2022, by = 1))

dusan_vlahovic_url <- (juve_df |> filter(player == 'Dušan Vlahović'))$player_url
dusan_vlahovic_df <- get_TM_player(player_url = dusan_vlahovic_url,
                                   user_agent = 'Dom Di Francesco, personal project')

player_goals_plot <- function(player_df){
  ggplot(data = dusan_vlahovic_df, mapping = aes(x = season, y = goals))+
    geom_point(mapping = aes(col = competition), alpha = 2/3)+
    facet_wrap(facets = ~team)+
    scale_colour_viridis_d()+
    scale_x_discrete(name = 'Season', limits = rev)+
    scale_y_continuous(name = 'Goals scored')+
    ggthemes::theme_base(base_size = 9, base_family = 'Atkinson Hyperlegible') +
    theme(plot.background = element_rect(colour = NA),
          axis.text.x = element_text(angle = 90),
          legend.title = element_blank())
}
player_goals_plot(player_df = dusan_vlahovic_df)
