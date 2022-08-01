# tmData
An R package that returns squad and player data from popular football (soccer) site transfermarkt.com.

To install the package, use the below code in R:

`require(devtools); devtools::install_github(repo = 'DomDF/tmData')`

This will allow you to use the functions:

`get_TM_squad()` and `get_TM_player()`

These functions use the polite package (https://github.com/dmi3kno/polite) to scrape data from squad and player pages. Some examples are provided below:

`juve_squad <- get_TM_squad(squad_url = 'https://www.transfermarkt.co.uk/juventus-turin/startseite/verein/506', user_agent = 'John Smith personal project')`

`dusan_vlahovic <- get_TM_player(player_url = 'https://www.transfermarkt.co.uk/du-scaron-an-vlahović/profil/spieler/357498', user_agent = 'John Smith personal project')`

Processed data is returned in tidy tibbles, ready for plotting, and tidyverse (https://www.tidyverse.org) data analysis.
