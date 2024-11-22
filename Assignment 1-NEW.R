#-Assignment 1 New Version ------

# Main author:
# Zizhen Zhong
# 2024, October 03
# Edits by:
# Nicole Eaton (November 2024): condensed filtering and data exploration steps, added rarefaction curves, improved map

# STEP 1: Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
# install.packages("maps")
library(maps)
library(vegan)

# STEP 2: Load data from BOLD and make a private file (for version control)
# dfBOLD_Tardigrada <- read_tsv("http://v4.boldsystems.org/index.php/API_Public/combined?taxon=Tardigrada&format=tsv") # Accessed on 21 November 2024
# write_tsv(dfBOLD_Tardigrada,'./Tardigrada_data_file.tsv') # Save file to current working directory
df_Tardigrada <- read_tsv("./Tardigrada_data_file.tsv")
colnames(df_Tardigrada)
dfBOLD_Interest <- df_Tardigrada[, c(8, 47)] # Keep columns for bin_uri and lat
summary(dfBOLD_Interest)

# STEP 3: State the research questions
# Tardigrades are known for their cryptobiosis and remarkable ability to endure extreme environmental stressors. Does exposure to such extreme environments correlate with greater genetic diversity within Tardigrade populations compared to those in more temperate habitats?
# Explore the geographic distribution of Tardigrada samples in BOLD.

# STEP 4: Filter the latitude data and separate into groups of temperate conditions (within the latitude range) and extreme conditions (without the latitude range)
Filter_lat_data <- function(df, lat_cutoff) {
  filtered_df <- df %>%
    filter(!is.na(bin_uri)) %>%
    filter(!is.na(lat)) %>%
    mutate(Within_or_without_latitude_range = case_when(abs(lat) > 50 ~ "Without", abs(lat) <= 50 ~ "Within"))
  return(filtered_df)
}
lat_data <- Filter_lat_data(dfBOLD_Interest, 50)
for (i in c("Without", "Within")) {
  cat(c("Number of entries ", i, " the latitude range: ", sum(lat_data$Within_or_without_latitude_range == i), "\nNumber of unique bins ", i, " the latitude range: ", length(unique(subset(lat_data, Within_or_without_latitude_range == i)$bin_uri)), "\n"))
}
# RESULTS FROM STEP 4: There are 104 different BINs found in temperate condition, while 120 BINs found in extreme conditions. See STEP 6 for further analysis.

# STEP 5: Check if elevation data should be considered for the extreme conditions group
# Thought to self, If Tardigrada is not genetically more diverse to handle extreme conditions in terms of temperature wise, maybe the elevation of where they are located can give insight. Because some of these Tardigrada may be living in temperate region, but in extreme conditions such as very high in mountain or very deep in sea as anther type of extreme condition.
dfBOLD_elevation <- df_Tardigrada[, c(8, 51)] %>%
  filter(!is.na(bin_uri)) %>%
  filter(!is.na(elev))
summary(dfBOLD_elevation$elev)
# RESULTS FROM STEP 5: The mean elevation for Tardigrada samples in BOLD is 180.8 m. The min is 0 m and the max is 2100 m. These elevations are not considered extreme conditions. Elevation need not be factored into filtering Tardigrada samples from BOLD into extreme vs temperate conditions.

# STEP 6: Make rarefaction curves for the two groups (temperate and extreme conditions)
counted_lat_data <- lat_data %>%
  group_by(Within_or_without_latitude_range) %>%
  count(bin_uri)
spread_data <- pivot_wider(data = counted_lat_data, names_from = bin_uri, values_from = n)
spread_data[is.na(spread_data)] <- 0
spread_data <- subset(spread_data, select = -Within_or_without_latitude_range)
rarecurve(spread_data, xlab = "Individuals sampled", ylab = "BIN richness", main = "Rarefaction curves for Tardigrada samples from temperate (blue) and extreme conditions (green)", col = c("darkblue", "darkgreen"))
# RESULTS FROM STEP 6: Step 4 results indicated that there is a different amount of genetic diversity for Tardigrada living in extreme conditions and in temperate conditions. The rarefaction curves for both groups are still increasing while only decelerating very slowly (not close to approaching an asymptote). This suggests that both groups have many more bins to be identified by more sampling. Therefore, the results from step 4 should be reconsidered when there are more samples taken and a more accurate knowledge of how many BINs really exist in each group.

# STEP 7: Create a world map to explore how the Tardigrada samples are geographically distributed
# When trying to create a simple world map, I found insight from https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/.
# When making edits, I looked for tips from httpssarahleejane.github.iolearningr20140921plotting-data-points-on-maps-with-r.html
df_to_map <- df_Tardigrada[, c(8, 47, 48)] %>% # Make a new data frame with latitude and longitude data
  filter(!is.na(bin_uri)) %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(lon))
mapping <- map_data("world")
ggplot() +
  geom_polygon(data = mapping, aes(x = long, y = lat, group = group), fill = "lightblue") +
  geom_point(data = df_to_map, aes(x = lon, y = lat, colour = bin_uri), size = 1, alpha = I(1), show.legend = FALSE) +
  labs(title = "Distribution of Tardigrada samples in BOLD", x = "Longitude", y = "Latitude", caption = "Geographic distribution of Tardigrada samples in BOLD, coloured by BIN assignment")
# RESULTS FROM STEP 7: The results from Steps 4 and 6 cannot conclude that there is greater genetic diversity for Tardigrada living in extreme conditions than in temperate conditions because more sampling is required in both groups to draw a conclusion. This map shows that sampling covers a range of latitudes but is heavily concentrated on the European continent. Increasing sampling in the undersampled continents/regions may produce more accurate results about the genetic diversity of Tardigrada between extreme and temperate conditions.
