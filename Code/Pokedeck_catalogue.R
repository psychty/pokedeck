# Loading some packages 
packages <- c('easypackages','readxl', 'tidyr', 'dplyr', 'readr', 'jsonlite', 'httr', 'rvest', 'stringr', 'scales', 'ggplot2','sjmisc', 'beepr', 'keyring')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_store <- '~/Repositories/Pokedeck/Data'

#key_get('poke_key')

collection_data <- read_csv(paste0(local_store, '/pokedeck_raw.csv'),
                            , locale = locale(encoding = 'latin1'))  %>% 
  arrange(Name) %>% 
  mutate(Name = gsub("\\.", "", Name))

duplicate_nametype <- collection_data %>% 
  group_by(Name, Type) %>% 
  summarise(Records = n()) %>% 
  filter(Records > 1)

if(nrow(duplicate_nametype)>0){
  
  paste0('Check the raw data for duplicate names recorded.')
  
}

# Later on we discover that Eevee can evolve into lots of different pokemon, and that makes it tricky for us and we probably wont worry about all the options for eevee so I'm going to remove the issue here.
collection_data <- collection_data %>% 
  # filter(!str_detect(Name, 'Eevee')) %>% 
  filter(!str_detect(Name, 'Pokegear')) %>% # also struggling with pokegear (because its a non-pokemon maybe)
  mutate(Name_type = paste0(Name, '_', Type)) 

# TODO find overall pokemon data, then left join the hierarchy (evoles to/evolves from etc)

# PokemonTCG.io
# This is a free api for all cards, it does not require an api key but has rate limits so we may find we hit those at some point when we start looping through our raw_card_data

# Third-party application rate limits depend on your API key. By default, requests are limited to 20,000/day.
# If you aren’t using an API key, you are rate limited to 1000 requests a day, and a maxium of 30 per minute.

# We can query all sorts of parameters but most simply our query is looking for the name, we dont need to make it query the specific card ID or set ID as we're just looking to add the cards we need, not to build sets as collectors (aint nobody got time/money for that) 

# For now lets use name, but later we might query evolvesFrom to look through out basic and stage one pokemon to see what could be good to get.

attribute_x <- 'name'

# TODO multiple names is proving tricky
# query_x <- gsub(' ', '%20', collection_stage_basic$Name[1])

# at the moment two names do not work, i cannot get a query string build that does not contain the escape for the double quote needed to appease the API.
# url_json <- paste0("https://api.pokemontcg.io/v2/cards?q=", attribute_x, ":'", query_x, "'&select=subtypes,types,name,evolvesTo,images")
# 
# raw_card_data <- httr::GET(url_json) %>% 
#   httr::content()

# So for now, we will have to extract using the first name only (which will return a whole load of pokemon we dont need, but at least it should contain the one pokemon we do need)

# Basic pokemon ####
collection_stage_basic <- collection_data %>% 
  filter(Stage == 'Basic')

for(i in 1:nrow(collection_stage_basic)){

if(i == 1){ 
  Pokemon_api_df <- data.frame()
  }

query_x <- word(collection_stage_basic$Name[i])

# Fix for é characters
if(query_x == 'Flabébé'){
  
  query_x <- 'Flab'
  
}

query_string <- paste0("https://api.pokemontcg.io/v2/cards?q=", attribute_x, ":", query_x, "&select=subtypes,types,name,evolvesTo,evolvesFrom,images")

# Response of the query 
# GET(query_string)

raw_data <- GET(query_string) %>% 
  content()

# Issue - there are many results for most queries

# Issue - some results have more columns than others (making a straight up matrix approach unhelpful)
processed_df <- raw_data$data

if(length(processed_df) == 0){
  
  print(paste0('Check ', query_x, ' is spelled correctly in the file, it does not seem to be in the database'))
  
  beepr::beep(9)

}  
  
# df <- data.frame(matrix(unlist(processed_df), nrow=length(processed_df), byrow=TRUE))#

# as.data.frame(do.call(cbind, processed_df)) %>% View() still problematic when there are some results with multiple values

if(length(processed_df) != 0){

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

}

Pokemon_api_df <- Pokemon_api_df %>% 
  bind_rows(dummy_df)

if(i == nrow(collection_stage_basic)){
  
  beepr::beep(2)
  
}  

}

Basic_df <- Pokemon_api_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Type = types, Level = subtypes, Evolves_to = evolvesTo, Evolves_from = evolvesFrom) %>% 
  unique() %>% 
  mutate(Name = gsub("\\.", "", Name)) %>% 
  mutate(Name_type =  paste0(Name, '_', Type)) %>% 
  filter(Name_type %in% collection_stage_basic$Name_type) %>%
  filter(!Level %in% c('Baby','Restored')) %>% 
  group_by(Name_type) %>% 
  mutate(Times_appeared = n()) %>% 
  filter(!(Times_appeared > 1 & is.na(Evolves_to))) %>% 
  mutate(Final_stage = case_when(is.na(Evolves_to) ~ 'Final evolution',
                                 !is.na(Evolves_to) ~ 'Not final evolution')) 

setdiff(collection_stage_basic$Name, Basic_df$Name)

# Stage one ####

collection_stage_one <- collection_data %>% 
  filter(Stage == 'Stage 1')

for(i in 1:nrow(collection_stage_one)){
  
  if(i == 1){ 
    Pokemon_api_df <- data.frame()
  }
  
  query_x <- word(collection_stage_one$Name[i])
  
  # Fix for é characters
  if(query_x == 'Flabébé'){
    
    query_x <- 'Flab'
    
  }
  
  query_x = 'pikachu'
  query_string <- paste0("https://api.pokemontcg.io/v2/cards?q=", attribute_x, ":", query_x, "&select=subtypes,types,name,evolvesTo,evolvesFrom,images")
  
  # Response of the query 
  # GET(query_string)
  
  raw_data <- GET(query_string) %>% 
    content()
  
  # Issue - there are many results for most queries
  
  # Issue - some results have more columns than others (making a straight up matrix approach unhelpful)
  processed_df <- raw_data$data
  
  if(length(processed_df) == 0){
    
    print(paste0('Check ', query_x, ' is spelled correctly in the file, it does not seem to be in the database'))
    
    beepr::beep(9)
    
  }  
  
  # df <- data.frame(matrix(unlist(processed_df), nrow=length(processed_df), byrow=TRUE))#
  
  # as.data.frame(do.call(cbind, processed_df)) %>% View() still problematic when there are some results with multiple values
  
  if(length(processed_df) != 0){
    
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
    
  }
  
  Pokemon_api_df <- Pokemon_api_df %>% 
    bind_rows(dummy_df)
  
  if(i == nrow(collection_stage_one)){
    
    beepr::beep(2)
    
  }  
  
}
  
Stage_one_df <- Pokemon_api_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Type = types, Level = subtypes, Evolves_to = evolvesTo, Evolves_from = evolvesFrom) %>% 
  unique() %>% 
  mutate(Name = gsub("\\.", "", Name)) %>% 
  mutate(Name_type =  paste0(Name, '_', Type)) %>% 
  filter(Name_type %in% collection_stage_basic$Name_type) %>%
  filter(!Level %in% c('Baby','Restored')) %>% 
  group_by(Name_type) %>% 
  mutate(Times_appeared = n()) %>% 
  filter(!(Times_appeared > 1 & is.na(Evolves_to))) %>% 
  mutate(Final_stage = case_when(is.na(Evolves_to) ~ 'Final evolution',
                                 !is.na(Evolves_to) ~ 'Not final evolution')) %>% 
  mutate(Level = case_when(Name == 'Magmar' ~ 'Stage 1',
                           TRUE ~ Level))

# Stage two ####
collection_stage_two <- collection_data %>% 
  filter(Stage == 'Stage 2')

for(i in 1:nrow(collection_stage_two)){
  
  if(i == 1){ 
    Pokemon_api_df <- data.frame()
  }
  
  query_x <- word(collection_stage_two$Name[i])
  
  # Fix for é characters
  if(query_x == 'Flabébé'){
    
    query_x <- 'Flab'
    
  }
  
  query_string <- paste0("https://api.pokemontcg.io/v2/cards?q=", attribute_x, ":", query_x, "&select=subtypes,types,name,evolvesTo,evolvesFrom,images")
  
  # Response of the query 
  # GET(query_string)
  
  raw_data <- GET(query_string) %>% 
    content()
  
  # Issue - there are many results for most queries
  
  # Issue - some results have more columns than others (making a straight up matrix approach unhelpful)
  processed_df <- raw_data$data
  
  if(length(processed_df) == 0){
    
    print(paste0('Check ', query_x, ' is spelled correctly in the file, it does not seem to be in the database'))
    
    beepr::beep(4)
    
  }  
  
  # df <- data.frame(matrix(unlist(processed_df), nrow=length(processed_df), byrow=TRUE))#
  
  # as.data.frame(do.call(cbind, processed_df)) %>% View() still problematic when there are some results with multiple values
  
  if(length(processed_df) != 0){
    
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
    
  }
  
  Pokemon_api_df <- Pokemon_api_df %>% 
    bind_rows(dummy_df)
  
  if(i == nrow(collection_stage_two)){
    
    beepr::beep(2)
    
  }  
  
}

Stage_two_df <- Pokemon_api_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Type = types, Level = subtypes, Evolves_to = evolvesTo, Evolves_from = evolvesFrom) %>% 
  unique() %>% 
  mutate(Name = gsub("\\.", "", Name)) %>% 
  mutate(Name_type =  paste0(Name, '_', Type)) %>% 
  filter(Name_type %in% collection_stage_basic$Name_type) %>%
  filter(!Level %in% c('Baby','Restored')) %>% 
  group_by(Name_type) %>% 
  mutate(Times_appeared = n()) %>% 
  filter(!(Times_appeared > 1 & is.na(Evolves_to))) %>% 
  mutate(Final_stage = case_when(is.na(Evolves_to) ~ 'Final evolution',
                                 !is.na(Evolves_to) ~ 'Not final evolution')) 


# Restored pokemon ####
collection_stage_restored <- collection_data %>% 
  filter(Stage == 'Restored')

for(i in 1:nrow(collection_stage_restored)){
  
  if(i == 1){ 
    Pokemon_api_df <- data.frame()
  }
  
  query_x <- word(collection_stage_restored$Name[i])
  
  # Fix for é characters
  if(query_x == 'Flabébé'){
    
    query_x <- 'Flab'
    
  }
  
  query_string <- paste0("https://api.pokemontcg.io/v2/cards?q=", attribute_x, ":", query_x, "&select=subtypes,types,name,evolvesTo,evolvesFrom,images")
  
  # Response of the query 
  # GET(query_string)
  
  raw_data <- GET(query_string) %>% 
    content()
  
  # Issue - there are many results for most queries
  
  # Issue - some results have more columns than others (making a straight up matrix approach unhelpful)
  processed_df <- raw_data$data
  
  if(length(processed_df) == 0){
    
    print(paste0('Check ', query_x, ' is spelled correctly in the file, it does not seem to be in the database'))
    
    beepr::beep(4)
    
  }  
  
  # df <- data.frame(matrix(unlist(processed_df), nrow=length(processed_df), byrow=TRUE))#
  
  # as.data.frame(do.call(cbind, processed_df)) %>% View() still problematic when there are some results with multiple values
  
  if(length(processed_df) != 0){
    
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
    
  }
  
  Pokemon_api_df <- Pokemon_api_df %>% 
    bind_rows(dummy_df)
  
  if(i == nrow(collection_stage_restored)){
    
    beepr::beep(2)
    
  }  
  
}

Restored_df <- Pokemon_api_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Type = types, Level = subtypes, Evolves_to = evolvesTo, Evolves_from = evolvesFrom) %>% 
  unique() %>% 
  mutate(Name = gsub("\\.", "", Name)) %>% 
  mutate(Name_type =  paste0(Name, '_', Type)) %>% 
  filter(Name_type %in% collection_stage_basic$Name_type) %>%
  filter(!Level %in% c('Baby')) %>% 
  group_by(Name_type) %>% 
  mutate(Times_appeared = n()) %>% 
  filter(!(Times_appeared > 1 & is.na(Evolves_to))) %>% 
  mutate(Final_stage = case_when(is.na(Evolves_to) ~ 'Final evolution',
                                 !is.na(Evolves_to) ~ 'Not final evolution')) 

Basic_df
Stage_one_df
Stage_two_df
Restored_df

# Missing cards ####

# TODO Identify missing cards
basic_theoretical_top_down <- Stage_one_df %>% 
  filter(!is.na(Evolves_from)) %>% 
  pull(Evolves_from) %>% 
  unique

# Which basic pokemon are we missing? 
Missing_basic_card <- setdiff(basic_theoretical_top_down, Basic_df$Name)

# What stage one cards should we have based on our stage 2 collection
stage_one_theoretical_top_down <- Stage_two_df %>% 
  pull(Evolves_from) %>% 
  unique

# What stage one cards should we have based on our basic collection
stage_one_theoretical_bottom_up <- Basic_df %>% 
  filter(!is.na(Evolves_to)) %>% 
  pull(Evolves_to) %>% 
  unique

# Which stage one pokemon are we missing? 
Missing_stage_one_card_a <- setdiff(stage_one_theoretical_top_down, Stage_one_df$Name)

# Which stage one pokemon are we missing? 
Missing_stage_one_card_b <- setdiff(stage_one_theoretical_bottom_up, Stage_one_df$Name)

Missing_stage_one_card <- c(Missing_stage_one_card_a, Missing_stage_one_card_b) %>%
  unique

# Which stage 2 cards are we missing based on our stage one collection
stage_two_theoretical_bottom_up <- Stage_one_df %>% 
  filter(!is.na(Evolves_to)) %>% 
  pull(Evolves_to) %>% 
  unique

# Which stage one pokemon are we missing? 
Missing_stage_two_card <- setdiff(stage_two_theoretical_bottom_up, Stage_two_df$Name)

# This will show us missing cards one up or one below each of our cards.
Total_missing_names <- c(Missing_basic_card, Missing_stage_one_card, Missing_stage_two_card) %>% 
  unique()

# TODO I'm not sure how it would behave or tell us about the final evolution (stage 2) of our basic cards if we didn't have stage 1 card. 

# Get data on missing cards ####
for(i in 1:length(Total_missing_names)){
  
  if(i == 1){ 
    Pokemon_api_df <- data.frame()
  }
  
  query_x <- word(Total_missing_names[i])
  
  query_string <- paste0("https://api.pokemontcg.io/v2/cards?q=", attribute_x, ":", query_x, "&select=subtypes,types,name,evolvesTo,evolvesFrom,images")
  
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
  
  Pokemon_api_df <- Pokemon_api_df %>% 
    bind_rows(dummy_df)
  
}

Needs_df <- Pokemon_api_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Type = types, Level = subtypes, Evolves_to = evolvesTo, Evolves_from = evolvesFrom) %>% 
  unique() %>% 
  mutate(Name_type = paste0(Name, '_', Type)) %>% 
  filter(Name %in% Total_missing_names) %>% 
  filter(!Level %in% c('Baby','Restored')) %>% 
  group_by(Name) %>% 
  mutate(Times_appeared = n()) %>% 
  filter(!(Times_appeared > 1 & is.na(Evolves_to))) %>% 
  mutate(Final_stage = case_when(is.na(Evolves_to) ~ 'Final evolution',
                                 !is.na(Evolves_to) ~ 'Not final evolution')) %>% 
  mutate(Level = case_when(Name == 'Magmortar' ~ 'Stage 2',
                           TRUE ~ Level))

# Does the needs df (for the stage 2 pokemon) give us enough info to create our pokemon groups?

# Grouping pokemon and finding final evolution ####
# TODO Create a 'starting from' pokemon name so that we can group evolutions together.

# For the basic stage this is easy
Basic_df <- Basic_df %>% 
  mutate(Starting_name = Name)

# This is also pretty easy for stage one pokemon cards (as the evolved from is only one stage behind and you don't need to have the basic card for this information to be retrieved)
Stage_one_df <- Stage_one_df %>% 
  mutate(Starting_name = Evolves_from) 
  
# It is slightly trickier for stage two pokemon, when we don't have the stage one card (and as such, would not have got the evolving to stage one pokemon name). I think we can get around this

# TODO Create a value for wheather we can play the card or not based on whether we have the cards that evolve into the pokemon



