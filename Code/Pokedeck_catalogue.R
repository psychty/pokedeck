
# Loading some packages 
packages <- c('easypackages','readxl', 'tidyr', 'dplyr', 'readr', 'jsonlite', 'httr', 'rvest', 'stringr', 'scales', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_store <- '~/Repositories/Pokedeck/Data'

collection_data <- read_csv(paste0(local_store, '/TCGplayerCardList.csv')) %>% 
  filter(!Name %in% c("Basic Darkness Energy", "Basic Fire Energy", "Basic Grass Energy", "Basic Lightning Energy", "Basic Metal Energy", "Basic Psychic Energy", "Basic Water Energy")) # We have taken out the energy cards as we are interested in Pokemon only (not items/tools/energy/stadium)

# Later on we discover that Eevee can evolve into lots of different pokemon, and that makes it tricky for us and we probably wont worry about all the options for eevee so I'm going to remove the issue here.
collection_data <- collection_data %>% 
  filter(!str_detect(Name, 'Eevee'))

# TODO find overall pokemon data, then left join the hierarchy (evoles to/evolves from etc)

# PokemonTCG.io
# This is a free api for all cards, it does not require an api key but has rate limits so we may find we hit those at some point when we start looping through our raw_card_data

# Third-party application rate limits depend on your API key. By default, requests are limited to 20,000/day.
# If you aren’t using an API key, you are rate limited to 1000 requests a day, and a maxium of 30 per minute.

# We can query all sorts of parameters but most simply our query is looking for the name, we dont need to make it query the specific card ID or set ID as we're just looking to add the cards we need, not to build sets as collectors (aint nobody got time/money for that) 

# For now lets use name, but later we might query evolvesFrom to look through out basic and stage one pokemon to see what could be good to get.

attribute_x <- 'name'

#TODO multiple names is proving tricky
query_x <- gsub(' ', '%20', collection_data$Name[1])

# at the moment two names do not work, i cannot get a query string build that does not contain the escape for the double quote needed to appease the API.
# url_json <- paste0("https://api.pokemontcg.io/v2/cards?q=", attribute_x, ":'", query_x, "'&select=subtypes,types,name,evolvesTo,images")
# 
# raw_card_data <- httr::GET(url_json) %>% 
#   httr::content()


# So for now, we will have to extract using the first name only (which will return a whole load of pokemon we dont need, but at least it should contain the one pokemon we do need)

for(i in 1:nrow(collection_data)){

if(i == 1){ 
  Pokemon_df <- data.frame()
  }

query_x <- word(collection_data$`Simple Name`[i])

query_string <- paste0("https://api.pokemontcg.io/v2/cards?q=", attribute_x, ":", query_x, "&select=subtypes,types,name,evolvesTo,images")

# Response of the query 
# GET(query_string)

raw_data <- GET(query_string) %>% 
  content()

# Issue - there are many results for most queries

# Issue - some results have more columns than others (making a straight up matrix approach unhelpful)
processed_df <- raw_data$data
# df <- data.frame(matrix(unlist(processed_df), nrow=length(processed_df), byrow=TRUE))#

# as.data.frame(do.call(cbind, processed_df)) %>% View() still problematic when there are some results with multiple values

for(j in 1:length(processed_df)) {

if(j == 1){
  dummy_df <- data.frame()
}
  
  processed_df_x <-  processed_df[j] %>%  
  unlist() %>% 
  t() %>% 
  as.data.frame()

dummy_df <- dummy_df %>% 
  bind_rows(processed_df_x)

}

Pokemon_df <- Pokemon_df %>% 
  bind_rows(dummy_df)

}

Pokemon_df_final <- Pokemon_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Level = subtypes, Type = types, EvolvesTo = evolvesTo) %>% 
  unique() %>% 
  filter(Name %in% collection_data$Name)

# This is a pretty good start. Though some basic pokemon appear twice, once with an EvolvesTo value and once without
# TODO identify duplicates and keep only the one which has a value for EvolvesTo.

# TODO left join Pokemon_df_final to collection to identify what each Basic and Stage 1 pokemon evolves to if it does.
# TODO Identify Stage 1 Pokemon (and Stage 2 Pokemon) in the EvolvesTo field to see what they evolved from.
# TODO Create a 'do we have evolved to' 'do we have evolved from'


