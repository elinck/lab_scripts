library(tidyverse) # load tidyverse data science libraries

# read in csv of GBIF occurences
data <- read_csv("data/occurences.csv")

# look at "head" of data
head(data)

# identify dimensions: first column is number of rows, second column is number of columns
dim(data)

# another way to look at attributes ("$" shows you what fields you can access)
str(data)

# we can subset columns we are interested in with "select":
data %>% 
  select(class, species, stateProvi, decimalLat, decimalLon, year)

# we can also filter by row value, e.g. state: 
data %>% 
  select(class, species, stateProvi, decimalLat, decimalLon, year) %>%
  filter(stateProvi %in% c("Montana", "North Dakota", "South Dakota")) 

# however, it's always good to check what values we actually have:
data %>%
  select(stateProvi) %>%
  unique() %>%
  print(n=100) # displays more rows than default

# filtering by year is similar: 
data %>% 
  select(class, species, stateProvi, decimalLat, decimalLon, year) %>%
  filter(stateProvi %in% c("Montana", "North Dakota", "South Dakota")) %>%
  filter(year < 1930)

# we can save a subset dataframe after applying these filters using "<-" (note different name)
subset_data <- data %>% 
  select(class, species, stateProvi, decimalLat, decimalLon, year) %>%
  filter(stateProvi %in% c("Montana", "North Dakota", "South Dakota")) %>%
  filter(year < 1930)
subset_data

# let's count the number of records (observations) in each taxonomic class class
subset_data %>%
  group_by(class, stateProvi) %>%
  summarise(n = n())

# we can do the same for species with "n_distinct(column)"
subset_data %>%
  group_by(class, stateProvi) %>%
  summarise(n = n_distinct(species))

# let's save this as its own object so we can export it
table <- subset_data %>%
  group_by(class, stateProvi) %>%
  summarise(n = n_distinct(species))
write_csv(table, "data/distinct_species_by_state.csv")

# let's also revisit filtering to produce a csv with only Montana records for plotting in a separate script
# I've updated this to include locality info (05/02/25)
montana_data <- data %>%
  select(class, species, stateProvi, locality, decimalLat, decimalLon, year, institutio) %>%
  filter(stateProvi == "Montana")
write_csv(montana_data, "data/montana_records.csv")

# to select sites, we may want to organize localities by the number of records in each class...
montana_data %>%
  group_by(class, locality) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# ...or species in each class
montana_data %>%
  group_by(class, locality) %>%
  summarise(n = n_distinct(species)) %>%
  arrange(desc(n))

# we can also do this for each class separately, saving the files as needed
bird_count_by_site <- montana_data %>%
  filter(class=="Aves") %>% # change for mammals, amphibians, herps, etc
  group_by(locality, decimalLat, decimalLon) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
write_csv(bird_count_by_site, "data/bird_count_by_site_mt.csv")

# we can also do this for each class separately, saving the files as needed
bird_species_by_site <- montana_data %>%
  filter(class=="Aves") %>%  # change for mammals, amphibians, herps, etc
  group_by(locality, decimalLat, decimalLon) %>%
  summarise(n = n_distinct(species))
write_csv(bird_species_by_site, "data/bird_species_by_site_mt.csv")




