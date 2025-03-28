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
# We don't want this to run through 800 pokemon each time a new card is added, this is not good use of resources

collection_stage_basic <- collection_data %>% 
  filter(Stage == 'Basic')

# Read in the data that has already been processed
processed_basic <- read_csv(paste0(local_store, '/Processed_basic.csv'),
                            , locale = locale(encoding = 'latin1'))

# Check if there is anything left to process.
collection_stage_basic_to_process <- collection_stage_basic %>% 
  filter(Name != 'Flabébé') %>% 
  filter(!Name_type %in% processed_basic$Name_type)
  
# If there is nothing left to process then process_basic does not change and in the next section is ignored. 

# If there is something to process, then only the new cards are processed and they are appended to the processed_basic object before it is re-written to file.

if(nrow(collection_stage_basic_to_process) > 0) {

for(i in 1:nrow(collection_stage_basic_to_process)){

if(i == 1){ 
  Pokemon_api_df <- data.frame()
  }

query_x <- word(collection_stage_basic_to_process$Name[i])

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

if(i == nrow(collection_stage_basic_to_process)){
  
  beepr::beep(2)
  
}  

}

Basic_df <- Pokemon_api_df %>% 
  mutate(name = case_when(name == 'Nidoran ♀' ~ 'Nidoran male',
                          TRUE ~ name)) %>% 
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
  filter(!(Name == 'Galarian Meowth' & Evolves_to == 'Perrserker'))

setdiff(collection_stage_basic_to_process$Name, Basic_df$Name)

setdiff(collection_stage_basic_to_process$Name_type, Basic_df$Name_type)

processed_basic_new <- collection_stage_basic_to_process %>%
  left_join(Basic_df, by = c('Name', 'Type','Name_type')) %>% 
  select(!c(Level, Times_appeared)) %>% 
  filter(Name != 'Cascoon')

processed_basic <- processed_basic_new %>% 
  bind_rows(processed_basic) %>% 
  unique()

processed_basic %>%
  write.csv(., paste0(local_store, '/Processed_basic.csv'),
            row.names = FALSE)

}

# Stage one ####

collection_stage_one <- collection_data %>% 
  filter(Stage == 'Stage 1')

# Read in the data that has already been processed
processed_s1 <- read_csv(paste0(local_store, '/Processed_stage_one.csv'),
                                   , locale = locale(encoding = 'latin1'))

collection_stage_one_to_process <- collection_stage_one %>% 
  filter(Name != 'Flabébé') %>% 
  filter(!Name_type %in% processed_s1$Name_type)

# If there is nothing left to process then processed_s1 does not change and in the next section is ignored. 

# If there is something to process, then only the new cards are processed and they are appended to the processed_s1 object before it is re-written to file.
       
if(nrow(collection_stage_one_to_process) > 0) {
       
for(i in 1:nrow(collection_stage_one_to_process)){
  
  if(i == 1){ 
    Pokemon_api_df <- data.frame()
  }
  
  query_x <- word(collection_stage_one_to_process$Name[i])
  
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
  
  if(i == nrow(collection_stage_one_to_process)){
    
    beepr::beep(2)
    
  }  
  
}
  
Stage_one_df <- Pokemon_api_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Type = types, Level = subtypes, Evolves_to = evolvesTo, Evolves_from = evolvesFrom) %>% 
  unique() %>% 
  mutate(Name = gsub("\\.", "", Name)) %>% 
  mutate(Name_type =  paste0(Name, '_', Type)) %>% 
  filter(Name_type %in% collection_stage_one_to_process$Name_type) %>%
  filter(!Level %in% c('Baby','Restored')) %>% 
  group_by(Name_type) %>% 
  mutate(Times_appeared = n()) %>% 
  filter(!(Times_appeared > 1 & is.na(Evolves_to))) %>% 
  mutate(Final_stage = case_when(is.na(Evolves_to) ~ 'Final evolution',
                                 !is.na(Evolves_to) ~ 'Not final evolution')) %>% 
  mutate(Level = case_when(Name == 'Magmar' ~ 'Stage 1',
                           TRUE ~ Level))

setdiff(collection_stage_one_to_process$Name, Stage_one_df$Name)

setdiff(collection_stage_one_to_process$Name_type, Stage_one_df$Name_type)

processed_stage_one_new <- collection_stage_one_to_process %>%
  left_join(Stage_one_df, by = c('Name', 'Type','Name_type')) %>% 
  select(!c(Level, Times_appeared)) %>% 
  filter(Evolves_from != 'Claw Fossil' | is.na(Evolves_from))

processed_s1 <- processed_stage_one_new %>% 
  bind_rows(processed_s1) %>% 
  unique()

processed_s1 %>% 
  write.csv(., paste0(local_store, '/Processed_stage_one.csv'),
            row.names = FALSE)

}

# Stage two ####
collection_stage_two <- collection_data %>% 
  filter(Stage == 'Stage 2')

# Read in the data that has already been processed
processed_s2 <- read_csv(paste0(local_store, '/Processed_stage_two.csv'),
                         , locale = locale(encoding = 'latin1'))

collection_stage_two_to_process <- collection_stage_two %>% 
  filter(Name != 'Flabébé') %>% 
  filter(!Name_type %in% processed_s2$Name_type)

# If there is nothing left to process then process_basic does not change and in the next section is ignored. 

# If there is something to process, then only the new cards are processed and they are appended to the processed_basic object before it is re-written to file.

if(nrow(collection_stage_two_to_process) > 0) {

for(i in 1:nrow(collection_stage_two_to_process)){
  
  if(i == 1){ 
    Pokemon_api_df <- data.frame()
  }
  
  query_x <- word(collection_stage_two_to_process$Name[i])
  
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
  
  if(i == nrow(collection_stage_two_to_process)){
    
    beepr::beep(2)
    
  }  
  
}

Stage_two_df <- Pokemon_api_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Type = types, Level = subtypes, Evolves_to = evolvesTo, Evolves_from = evolvesFrom) %>% 
  unique() %>% 
  mutate(Name = gsub("\\.", "", Name)) %>% 
  mutate(Name_type =  paste0(Name, '_', Type)) %>% 
  filter(Name_type %in% collection_stage_two_to_process$Name_type) %>%
  filter(!Level %in% c('Baby','Restored')) %>% 
  group_by(Name_type) %>% 
  mutate(Times_appeared = n()) %>% 
  filter(!(Times_appeared > 1 & is.na(Evolves_to))) %>% 
  mutate(Final_stage = case_when(is.na(Evolves_to) ~ 'Final evolution',
                                 !is.na(Evolves_to) ~ 'Not final evolution')) 

setdiff(collection_stage_two_to_process$Name, Stage_two_df$Name)
setdiff(collection_stage_two_to_process$Name_type, Stage_two_df$Name_type)

processed_s2_new <- collection_stage_two_to_process %>%
  left_join(Stage_two_df, by = c('Name', 'Type','Name_type')) %>% 
  select(!c(Level, Times_appeared)) 

processed_s2 <- processed_s2_new %>% 
  bind_rows(processed_s2) %>% 
  unique()

processed_s2 %>% 
  write.csv(., paste0(local_store, '/Processed_stage_two.csv'),
            row.names = FALSE)

}

# Restored pokemon ####
collection_stage_restored <- collection_data %>% 
  filter(Stage == 'Restored')

# Read in the data that has already been processed
processed_restored <- read_csv(paste0(local_store, '/Processed_restored.csv'),
                         , locale = locale(encoding = 'latin1'))

collection_stage_restored_to_process <- collection_stage_restored %>% 
  filter(Name != 'Flabébé') %>% 
  filter(!Name_type %in% processed_restored$Name_type)

# If there is nothing left to process then process_basic does not change and in the next section is ignored. 

# If there is something to process, then only the new cards are processed and they are appended to the processed_basic object before it is re-written to file.

if(nrow(collection_stage_restored_to_process) > 0) {

for(i in 1:nrow(collection_stage_restored_to_process)){
  
  if(i == 1){ 
    Pokemon_api_df <- data.frame()
  }
  
  query_x <- word(collection_stage_restored_to_process$Name[i])
  
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
  
  if(i == nrow(collection_stage_restored_to_process)){
    
    beepr::beep(2)
    
  }  
  
}

Restored_df <- Pokemon_api_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Type = types, Level = subtypes, Evolves_to = evolvesTo, Evolves_from = evolvesFrom) %>% 
  unique() %>% 
  mutate(Name = gsub("\\.", "", Name)) %>% 
  mutate(Name_type =  paste0(Name, '_', Type)) %>% 
  filter(Name_type %in% collection_stage_restored_to_process$Name_type) %>%
  filter(Level %in% c('Restored')) %>% 
  group_by(Name_type) %>% 
  mutate(Times_appeared = n()) %>% 
  filter(!(Times_appeared > 1 & is.na(Evolves_to))) %>% 
  mutate(Final_stage = case_when(is.na(Evolves_to) ~ 'Final evolution',
                                 !is.na(Evolves_to) ~ 'Not final evolution')) 

setdiff(collection_stage_restored_to_process$Name, Restored_df$Name)
setdiff(collection_stage_restored_to_process$Name_type, Restored_df$Name_type)

processed_restored_new <- collection_stage_restored_to_process %>%
  left_join(Restored_df, by = c('Name', 'Type','Name_type')) %>% 
  select(!c(Level, Times_appeared)) 

processed_restored <- processed_restored_new %>% 
  bind_rows(processed_restored) %>% 
  unique()

processed_restored %>% 
  write.csv(., paste0(local_store, '/Processed_restored.csv'),
            row.names = FALSE)

}

# These should be the most up to date versions of our objects

processed_basic
processed_s1
processed_s2
processed_restored

rm(collection_stage_basic, collection_stage_basic_to_process, collection_stage_one, collection_stage_one_to_process, collection_stage_two, collection_stage_two_to_process, collection_stage_restored, collection_stage_restored_to_process, duplicate_nametype)

# Missing cards ####

# TODO Identify missing cards
basic_theoretical_top_down <- processed_s1 %>% 
  filter(!is.na(Evolves_from)) %>% 
  pull(Evolves_from) %>% 
  unique

# Which basic pokemon are we missing? 
Missing_basic_card <- setdiff(basic_theoretical_top_down, processed_basic$Name)

# Expect this to be zero
collection_data %>% 
  filter(Name %in% Missing_basic_card)

# What stage one cards should we have based on our stage 2 collection
stage_one_theoretical_top_down <- processed_s2 %>% 
  pull(Evolves_from) %>% 
  unique

# What stage one cards should we have based on our basic collection
stage_one_theoretical_bottom_up <- processed_basic %>% 
  filter(!is.na(Evolves_to)) %>% 
  pull(Evolves_to) %>% 
  unique

# Which stage one pokemon are we missing? 
Missing_stage_one_card_a <- setdiff(stage_one_theoretical_top_down, processed_s1$Name)

# Which stage one pokemon are we missing? 
Missing_stage_one_card_b <- setdiff(stage_one_theoretical_bottom_up, processed_s1$Name)

Missing_stage_one_card <- c(Missing_stage_one_card_a, Missing_stage_one_card_b) %>%
  unique

# There are one or two edge cases here - Clefairy is a basic and a stage one card theoretically (as Cleffa was introduced also as basic but evoles into Clefairy)
Missing_stage_one_card <- Missing_stage_one_card[! Missing_stage_one_card %in% c('Clefairy', 'Electabuzz')]

rm(Missing_stage_one_card_a, Missing_stage_one_card_b)

# Expect this to be zero
collection_data %>% 
  filter(Name %in% Missing_stage_one_card)

# Which stage 2 cards are we missing based on our stage one collection
stage_two_theoretical_bottom_up <- processed_s1 %>% 
  filter(!is.na(Evolves_to)) %>% 
  pull(Evolves_to) %>% 
  unique

# Which stage one pokemon are we missing? 
Missing_stage_two_card <- setdiff(stage_two_theoretical_bottom_up, processed_s2$Name)

# Expect this to be zero
collection_data %>% 
  filter(Name %in% Missing_stage_two_card)

# This will show us missing cards one up or one below each of our cards.
Total_missing_names <- c(Missing_basic_card, Missing_stage_one_card, Missing_stage_two_card) %>% 
  unique() 

Total_missing_names %>% print

# Read in the data that has already been processed
Processed_needs <- read_csv(paste0(local_store, '/Processed_needs.csv'),
                            , locale = locale(encoding = 'latin1'))

Needs_to_process <- Total_missing_names[! Total_missing_names %in% Processed_needs$Name]

# If there is nothing left to process then processed_s1 does not change and in the next section is ignored. 

# If there is something to process, then only the new cards are processed and they are appended to the processed_s1 object before it is re-written to file.

if(length(Needs_to_process) > 0) {

# Get data on missing cards ####
for(i in 1:length(Needs_to_process)){
  
  if(i == 1){ 
    Pokemon_api_df <- data.frame()
  }
  
  query_x <- word(Needs_to_process[i])
  
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

Processed_needs_new <- Pokemon_api_df %>% 
  mutate(subtypes = ifelse(is.na(subtypes), subtypes1, subtypes)) %>% 
  select(Name = name, Type = types, Level = subtypes, Evolves_to = evolvesTo, Evolves_from = evolvesFrom) %>% 
  unique() %>% 
  mutate(Name_type = paste0(Name, '_', Type)) %>% 
  filter(Name %in% Needs_to_process) %>% 
  group_by(Name) %>% 
  mutate(Final_stage = case_when(is.na(Evolves_to) ~ 'Final evolution',
                                 !is.na(Evolves_to) ~ 'Not final evolution')) %>% 
  mutate(Level = case_when(Name == 'Magmortar' ~ 'Stage 2',
                           TRUE ~ Level)) %>% 
  select(!c(Type, Name_type)) %>% unique()

Processed_needs <- Processed_needs_new %>% 
  bind_rows(Processed_needs) %>% 
  unique()

}

Processed_needs %>% 
  filter(!Name %in% collection_data$Name) %>% # clear out any new cards added to the collection (this should eventually reduce this object to near zero)
  mutate(Quantity = 0) %>% 
  write.csv(., paste0(local_store, '/Processed_needs.csv'),
            row.names = FALSE)

# Still some that are quirky
  data.frame(Name = Needs_to_process) %>% 
    mutate(Quantity = 0)

# I want to be able to search a list and say whether it is a card needed (and if theres no match then maybe you could buy the card but this could add to the collection).

processed_basic %>% 
  bind_rows(processed_s1) %>% 
  bind_rows(processed_s2) %>% 
  bind_rows(processed_restored) %>% 
  bind_rows(Processed_needs) %>% 
  bind_rows( data.frame(Name = Needs_to_process) %>% 
               mutate(Quantity = 0)) %>% 
  toJSON() %>% 
  write_lines(paste0(local_store,'/collection_values.json'))


# Can you get a whole list ####

# Ordinarily we'd like to use the most recently available release of patients, and could use the following code to extract the latest.
calls_webpage <- read_html('https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_name') %>%
  html_nodes("a") %>%
  html_attr("href")

# # we know the actual page we want has a url which starts with the following string, so reduce the scraped list above to those which include it
calls_pokemon <- unique(grep('Pok', calls_webpage, value = T))

bulbapedia_df <- as.data.frame(calls_pokemon) %>% 
  rename(Name = calls_pokemon) %>% 
  filter(str_detect(Name, '^/wiki/')) %>% 
  mutate(Name = strex::str_after_first(Name, '/wiki/')) %>% 
  mutate(Name = strex::str_before_last(Name, '_')) %>% 
  filter(Name != 'Pok%C3%A9mon') %>% 
  mutate(Name = gsub('_', ' ', gsub('\\.', '', Name))) %>% 
  unique()

bulbapedia_df %>% 
  write.csv(., paste0(local_store, '/bulbapedia_full_name_list.csv'),
            row.names = FALSE)


collection_data %>% filter(Name %in% bulbapedia_df$Name) %>% 
  select(Name) %>% unique %>% nrow


collection_data %>% 
  pull(Name) %>% unique %>% length


setdiff(collection_data$Name, bulbapedia_df$Name)


# 
# # We also know that the top result will be the latest version (even though the second result is the next upcoming version)
# calls_patient_numbers_webpage <- read_html(paste0('https://digital.nhs.uk/',calls_patient_numbers_webpage[1])) %>%
#   html_nodes("a") %>%
#   html_attr("href")

###################################################
# Grouping pokemon and finding final evolution ####
###################################################

# TODO Create a 'starting from' pokemon name so that we can group evolutions together.
# 
# # For the basic stage this is easy
# processed_basic <- processed_basic %>% 
#   mutate(Starting_name = Name)
# 
# # This is also pretty easy for stage one pokemon cards (as the evolved from is only one stage behind and you don't need to have the basic card for this information to be retrieved)
# processed_s1 <- processed_s1 %>% 
#   mutate(Starting_name = Evolves_from) 
#   
# # It is slightly trickier for stage two pokemon, when we don't have the stage one card (and as such, would not have got the evolving to stage one pokemon name). I think we can get around this
# 
# one_form_pokemon <- processed_basic %>% 
#   select(Starting_name, Final_stage) %>% 
#   unique() %>% 
#   filter(Final_stage == 'Final evolution') 
# 
# dual_form_pokemon <- processed_basic %>% 
#   filter(!Starting_name %in% one_form_pokemon$Starting_name) %>% 
#   select(Starting_name) %>%
#   unique() %>% 
#   left_join(processed_s1[c('Starting_name', 'Name', 'Final_stage')], by = 'Starting_name') %>% 
#   rename('Stage_one_name' = 'Name') %>% 
#   filter(Final_stage == 'Final evolution')
# 
# 
# one_form_pokemon %>% 
#   bind_rows(dual_form_pokemon) %>% 
#   View()

# TODO Create a value for wheather we can play the card or not based on whether we have the cards that evolve into the pokemon

processed_s1[c('Name', 'Evolves_to')] %>% View()

processed_basic %>% 
  select(Basic_name = Name, Stage_one_name = Evolves_to) %>% 
  left_join(processed_s1[c('Name', 'Evolves_to')], by = c('Stage_one_name' = 'Name')) %>% 
  View()

