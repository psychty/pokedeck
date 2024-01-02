
# Loading some packages 
packages <- c('easypackages','readxl', 'tidyr', 'dplyr', 'readr', 'jsonlite', 'httr', 'rvest', 'stringr', 'scales', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_store <- '~/Repositories/Pokedeck/Data'

raw_data <- read_csv(paste0(local_store, '/TCGplayerCardList.csv'))

# TODO find overall pokemon data, then left join the hierarchy (evoles to/evolves from etc)

# PokemonTCG.io
# This is a free api for all cards, it does not require an api key but has rate limits so we may find we hit those at some point when we start looping through our raw_card_data

# Third-party application rate limits depend on your API key. By default, requests are limited to 20,000/day.
# If you aren’t using an API key, you are rate limited to 1000 requests a day, and a maxium of 30 per minute.

# We can query all sorts of parameters but most simply our query is looking for the name, we dont need to make it query the specific card ID or set ID as we're just looking to add the cards we need, not to build sets as collectors (aint nobody got time/money for that) 

# For now lets use name, but later we might query evolvesFrom to look through out basic and stage one pokemon to see what could be good to get.

attribute_x <- 'name'
#TODO multiple names is proving tricky
query_x <- gsub(' ', '%20', raw_data$Name[1])

# query_x <- 'Grimer'

url_json <- paste0('https://api.pokemontcg.io/v2/cards?q=', attribute_x, ':"', query_x, '"&select=subtypes,types,name,evolvesTo,images')

raw_card_data <- httr::GET(url_json) %>% 
  httr::content()

url_json

