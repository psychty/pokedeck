
# Loading some packages 
packages <- c('easypackages','readxl', 'tidyr', 'dplyr', 'readr', 'jsonlite', 'httr', 'rvest', 'stringr', 'scales', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_store <- '~/Repositories/Pokedeck/Data'

collection_data <- read_csv(paste0(local_store, '/TCGplayerCardList.csv'))

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

for(i in 1:10){

if(i == 1){ 
  Pokemon_df <- data.frame()}

query_x <- word(collection_data$Name[i])

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
  unique()

