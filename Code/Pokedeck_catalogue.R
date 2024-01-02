
# Loading some packages 
packages <- c('easypackages','readxl', 'tidyr', 'dplyr', 'readr', 'jsonlite', 'httr', 'rvest', 'stringr', 'scales', 'ggplot2')
install.packages(setdiff(packages, rownames(installed.packages())))
easypackages::libraries(packages)

local_store <- '~/Repositories/Pokedeck/Data'

raw_data <- read_csv(paste0(local_store, '/TCGplayerCardList.csv'))

# TODO find overall pokemon data, then left join the hierarchy (evoles to/evolves from etc)