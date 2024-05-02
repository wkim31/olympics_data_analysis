require(tidyverse)

olympics <- read_csv('athlete_events.csv')
regions <- read_csv('noc_regions.csv')
gdp <- read_csv('gdp_1960_2020.csv')
pop <- read_csv('world_pop.csv')



#Contained VIZ assingment: 
olympics <- olympics %>%
  left_join(regions, by = join_by(NOC == NOC))

#contained viz:
grouped_countries <- olympics %>%
  select(Name, NOC, Year, Season, City, Medal, region) %>%
  group_by(NOC) %>%
  filter(!is.na(Medal))
  
#constuct host country:
host_country <- function(col) {
  if (col == "Rio de Janeiro") {
    return("Brazil")
  } else if (col == "London") {
    return("United Kingdom")
  } else if (col == "Beijing") {
    return("China")
  } else if (col == "Athina") {
    return("Greece")
  } else if (col == "Sydney" | col == "Melbourne") {
    return("Australia")
  } else if (col == "Atlanta" | col == "Los Angeles" | col == "St. Louis" | col == "Lake Placid") {
    return("USA")
  } else if (col == "Barcelona") {
    return("Spain")
  } else if (col == "Seoul") {
    return("South Korea")
  } else if (col == "Moskva") {
    return("Russia")
  } else if (col == "Montreal") {
    return("Canada")
  } else if (col == "Munich" | col == "Berlin" | col == "Garmisch-Partenkirchen") {
    return("Germany")
  } else if (col == "Mexico City") {
    return("Mexico")
  } else if (col == "Tokyo" | col == "Sapporo" | col == "Nagano") {
    return("Japan")
  } else if (col == "Roma") {
    return("Italy")
  } else if (col == "Paris") {
    return("France")
  } else if (col == "Helsinki") {
    return("Finland")
  } else if (col == "Amsterdam") {
    return("Netherlands")
  } else if (col == "Antwerpen") {
    return("Belgium")
  } else if (col == "Stockholm") {
    return("Sweden")
  } else if (col == "Sochi") {
    return("Russia")
  } else if (col == "Albertville" | col == "Chamonix" | col == "Grenoble") {
    return("France")
  } else if (col == "Lillehammer" | col == "Oslo") {
    return("Norway")
  } else if (col == "Salt Lake City" | col == "Squaw Valley") {
    return("USA")
  } else if (col == "Torino" | col == "Cortina d'Ampezzo") {
    return("Italy")
  } else if (col == "Sankt Moritz") {
    return("Switzerland")
  } else if (col == "Sarajevo") {
    return("Yugoslavia")
  } else if (col == "Calgary" | col == "Vancouver") {
    return("Canada")
  } else if (col == "Innsbruck") {
    return("Austria")
  }
  else {
    return("Other")
  }
}

# Applying this function
grouped_countries <- grouped_countries %>%
  mutate(Host_Country = sapply(City, host_country))


#other_countries <- grouped_countries %>%
  #filter(Host_Country == 'Other')

host_medals <- grouped_countries %>%
  filter(`region` == `Host_Country`) %>%
  group_by(region, Year) %>%
  summarize(sum_medals = n())

non_host_medals <- grouped_countries %>%
  filter(`region` != `Host_Country`) %>%
  group_by(region, Year) %>%
  summarize(sum_medals = n())


plot <- ggplot(host_medals, aes(x = sum_medals, y = region)) +
  geom_dotplot(stackdir = "center")
plot

