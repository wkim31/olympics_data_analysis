require(tidyverse)
require(sf)
require(cartogram)


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


###Chart 2: country x gdp:

selected_countries <- c('USA', 'China', 'Japan', 'Germany', 'UK', 'France', 'Russia', 'Italy', 'Sweden')

medal_count <- grouped_countries %>%
  filter(region %in% selected_countries) %>%
  group_by(region, Year) %>%
  summarize(total_medals = n())


medal_gdp <- left_join(medal_count, gdp, by = c("region" = "country"))

unwanted_years <- c(1994, 1998, 2002, 2006, 2010, 2014, 2018) #filter out winter olympics
medal_gdp <- medal_gdp %>%
  filter(!(Year %in% unwanted_years))

ggplot(medal_gdp, aes(x = Year, y = total_medals, color = region)) +
  geom_line() +
  facet_wrap(~ region, scales = "free_y", ncol = 3) +  #facet by country
  labs(title = "Medal Count Over Time by Country", x = "Year", y = "Total Medals") +
  geom_vline(xintercept = 1945, linetype = "dashed", color = "black")


#Chart 3: % of gold medals out of total gold medals for that year for selected countries


grouped_countries <- grouped_countries %>%
  filter(Season != "Winter") #filter out winter olympics

selected_countries <- c('USA', 'Switzerland', 'Japan', 'Germany', 'UK', 'Russia', 'Italy', 'Sweden', 'Argentina')
filtered_years <- 1910:1980

medal_count <- grouped_countries %>%
  filter(region %in% selected_countries & Year %in% filtered_years) %>%
  group_by(region, Year) %>%
  summarize(total_gold_medals = sum(Medal == "Gold")) %>%
  mutate(percentage_gold = total_gold_medals / sum(total_gold_medals) * 100)

#merge with the GDP data
medal_gdp <- left_join(medal_count, gdp, by = c("region" = "country", "Year" = "year"))

#create the faceted plot
ggplot(medal_gdp, aes(x = Year, y = percentage_gold, color = region)) +
  geom_line() +
  facet_wrap(~ region, scales = "free_y", ncol = 3) +  #facet by country
  labs(title = "Percentage of Gold Medals Out of Total Gold Medals Over Time by Country", x = "Year", y = "Percentage of Gold Medals") +
  geom_vline(xintercept = 1945, linetype = "dashed", color = "black")  #

#change order:
custom_order <- c("Russia", "UK", "USA", "Italy", "Germany", "Japan", "Switzerland", "Sweden", "Argentina")

#convert region to a factor with custom order
medal_gdp$region <- factor(medal_gdp$region, levels = custom_order)

#create the faceted plot with specified y-axis scale
ggplot(medal_gdp, aes(x = Year, y = percentage_gold, color = region)) +
  geom_line() +
  facet_wrap(~ region, scales = "free_y", ncol = 3, 
             labeller = labeller(region = function(x) x)) +  #facet by country
  labs(title = "Biggest Losers of WWII, in terms of Olympics: Neutral Countries",
       subtitle = "Percentage of gold medals out of all gold medals handed out, by year",
       x = "", 
       y = "", 
       caption = "Source: Kaggle dataset on the modern Olympic Games, including all the Games from Athens 1896 to Rio 2016.") +
  geom_vline(xintercept = 1945, linetype = "dashed", color = "black") +  #add vertical line at 1945
  ylim(0, 15)  #set y-axis scale from 0 to 20


# Filter the data to include only gold medals
gold_medals <- grouped_countries %>%
  filter(Medal == "Gold")

# Group the data by country and year, and count the number of gold medals
gold_medals_count <- gold_medals %>%
  group_by(region, Year) %>%
  summarize(gold_count = n())

# Define the order of countries for facetting
custom_order <- c("USA", "Switzerland", "Japan", "Germany", "UK", "Russia", "Italy", "Sweden", "Argentina")

# Convert region to a factor with custom order
gold_medals_count$region <- factor(gold_medals_count$region, levels = custom_order)

# Create the facet chart
ggplot(gold_medals_count, aes(x = Year, y = gold_count)) +
  geom_bar(stat = "identity", fill = "gold") +
  facet_wrap(~ region, scales = "free_y", ncol = 3) +
  labs(title = "Gold Medals Won Each Year by Country",
       x = "Year",
       y = "Number of Gold Medals",
       caption = "Source: Kaggle dataset on the modern Olympic Games") +
  theme_minimal()



#Map of Total Aggregate Medals by Country:
world <- st_read("world-administrative-boundaries/world-administrative-boundaries.shp")
world <- world %>%
  mutate(name = case_when(
    name == "United States of America" ~ "USA",
    name == "Russian Federation" ~ "Russia",
    name == "Iran (Islamic Republic of)" ~ "Iran",
    TRUE ~ as.character(name)
  ))

total_medals <- grouped_countries %>%
  group_by(region) %>%
  summarize(total_medals = n())

total_medals <- world %>%
  left_join(total_medals, by = c("name" = "region"))

map_total_med <- ggplot(total_medals, aes(fill = total_medals)) +
  geom_sf(color = NA) +
  coord_sf(crs = st_crs("+proj=robin")) +
  theme_void() +
  labs(title = "USA vs Russia: Modern Olympic Hegemons",
       subtitle = "Total number of medals accumulated by country",
       caption = "Source: Kaggle dataset on the modern Olympic Games, including all the Games from Athens 1896 to Rio 2016.")

map_total_med


#With breaks:
breaks <- c(0, 200, 400, 1000, 2000, 3000, 4000, 5000)

custom_colors <- c("#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

map_total_med <- ggplot(total_medals, aes(fill = total_medals)) +
  geom_sf(color = NA) +
  scale_fill_gradientn(colors = custom_colors, breaks = breaks, labels = scales::comma(breaks)) +
  coord_sf(crs = st_crs("+proj=robin")) +
  theme_void() +
  labs(title = "USA vs Russia: Modern Olympic Hegemons",
       subtitle = "Total number of medals accumulated by country",
       caption = "Source: Kaggle dataset on the modern Olympic Games, including all the Games from Athens 1896 to Rio 2016.")

map_total_med


#GDP x total medals:
total_medals <- grouped_countries %>%
  group_by(region) %>%
  mutate(region = case_when(
    region == "USA" ~ "the United States",
    region == "UK" ~ "United Kingdom",
    TRUE ~ as.character(region)
  )) %>%
  summarize(total_medals = n())

# Step 2: Merge total medals with GDP data
combined_data <- gdp %>%
  left_join(total_medals, by = c("country" = "region"))


##############

combined_data <- mutate(combined_data, GDP_level = cut(gdp, 
                                                       breaks = quantile(gdp, probs = c(0, 0.25, 0.5, 0.75, 1)),
                                                       labels = c("Low", "Lower Middle", "Upper Middle", "High"),
                                                       include.lowest = TRUE))

#RIDGE:
ridge <- ggplot(combined_data, aes(x = `total_medals`, y = `GDP_level`, fill = ..x..)) + #<- ..x.. tells it to fill by calories
  stat_density_ridges(geom = "density_ridges_gradient", scale = 1.5, rel_min_height = 0.01, alpha = 0.7) +
  scale_fill_gradient(low = "#cbd5e8", high = "#377eb8", guide = "legend") +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(
    title = 'Who Are Hogging all the Medals: High GDP Countries',
    subtitle = 'GDP levels vs total medals accrued by those countries, by year',
    x = '',
    y = "",
    caption = "Source: Kaggle") 

ridge


###

olympic_colors <- c("#0081C8", "#FCB131", "#000000", "#00A651", "#EE334E") # Blue, Yellow, Black, Green, Red

# Ridge plot with Olympic colors
ridge <- ggplot(combined_data, aes(x = total_medals, y = GDP_level, fill = ..x..)) +
  stat_density_ridges(geom = "density_ridges_gradient", scale = 1.5, rel_min_height = 0.01, alpha = 0.7) +
  scale_fill_gradient(low = olympic_colors[1], high = olympic_colors[5], guide = "legend") + # Use Olympic colors
  scale_x_continuous(limits = c(0, 1000)) +
  labs(
    title = 'Who Are Hogging all the Medals: High GDP Countries',
    subtitle = 'GDP levels vs total medals accrued by those countries, by year',
    x = '',
    y = "",
    caption = "Source: Kaggle"
  )

ridge


# Filter the data to include only gold medals
gold_medals <- grouped_countries %>%
  filter(Medal == "Gold")

gold_medals_count <- gold_medals %>%
  group_by(region, Year) %>%
  summarize(gold_count = n())

custom_order <- c("USA", "Switzerland", "Japan", "Germany", "UK", "Russia", "Italy", "Sweden", "Argentina")

gold_medals_count$region <- factor(gold_medals_count$region, levels = custom_order)

ggplot(gold_medals_count, aes(x = Year, y = gold_count)) +
  geom_bar(stat = "identity", fill = "gold") +
  facet_wrap(~ region, scales = "free_y", ncol = 3) +
  labs(title = "Gold Medals Won Each Year by Country",
       x = "Year",
       y = "Number of Gold Medals",
       caption = "Source: Kaggle dataset on the modern Olympic Games") +
  theme_minimal()


######
library(ggplot2)

selected_countries <- c("China", "South Korea", "Cuba", "Germany", "Russia", "Sweden")

selected_gold_medals <- gold_medals_count %>%
  filter(region %in% selected_countries)

#create a factor for region with custom levels to specify the order
selected_gold_medals$region <- factor(selected_gold_medals$region, levels = selected_countries)

ggplot(selected_gold_medals, aes(x = Year, y = gold_count, group = region, color = region, fill = region)) +
  geom_ribbon(aes(ymin = 0, ymax = gold_count), alpha = 0.2) + # Fill the area under the lines
  geom_line() +
  geom_point() +  
  facet_wrap(~ region, scales = "free_y", nrow = 2) +
  labs(title = "Exceptions to the GDP Rule: Challenging the Status Quo",
       subtitle = "Gold Medals Won Each Year by Selected Countries",
       x = "",
       y = "",
       caption = "Source: Kaggle dataset on the modern Olympic Games, 1896-2016") +
  theme_minimal() +
  scale_fill_manual(values = c("China" = "#808080", "South Korea" = "#808080", "Cuba" = "#004B87", 
                               "Germany" = "#808080", "Russia" = "#808080", "Sweden" = "#FECC02")) +
  scale_color_manual(values = c("China" = "#808080", "South Korea" = "#808080", "Cuba" = "#DA291C", 
                                "Germany" = "#808080", "Russia" = "#808080", "Sweden" = "#006AA7")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

