install.packages("maps")
install.packages("mapproj") 
install.packages("patchwork")
library(patchwork)
library(maps) #package to create the US map
library(mapproj)
library(tidyverse)
library(ggplot2)
###
data <- read_csv('https://course-resources.minerva.edu/uploaded_files/mke/Y6azen/howe-2016-data.csv')
metadata <- read_csv('https://course-resources.minerva.edu/uploaded_files/mke/nB7zEY/howe-2016-metadata.csv')
### get only the rows in which GeoType are states or counties, and stores them into variables
stateslevel <- filter(data, data$GeoType == 'State')
countieslevel <- filter(data, data$GeoType == 'County')
#splits the GeoName column to seprate State and County Name in the countieslevel dataframe
countieslevel <- countieslevel %>%
  separate(GeoName, into = c("CountyName", "StateName"), sep = ",", extra = "merge")
head(countieslevel)
#makes the state name lower case in stateslevel dataframe 
stateslevel$GeoName <- tolower(stateslevel$GeoName)
#new columns that stores the difference from national average
stateslevel$Meanhappening <-stateslevel$happening - 70.151
stateslevel$MeanhappeningOppose <-stateslevel$happeningOppose - 12.427
#new column that calculates the percentage of people who do not know that global warming is happening.
stateslevel$DNhappening <- 100 - (stateslevel$happening + stateslevel$happeningOppose)
stateslevel$MeanDNhappening <- (stateslevel$DNhappening - (100 - (70.151 + 12.427)))
#map data for the states
statesmap <- map_data("state")
# Renaming 'OldName' to 'NewName'
colnames(statesmap)[colnames(statesmap) == "region"] <- "GeoName"
# merge and sort (plots in order, sort ensures states filled in)
stateslevel.geo <- merge(statesmap,stateslevel , sort = FALSE, by = "GeoName")
stateslevel.geo <- stateslevel.geo[order(stateslevel.geo$order), ]

p1 <- ggplot(stateslevel.geo, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = Meanhappening), color = "black", size = 0.25) + 
  coord_map() + theme_void() + scale_fill_gradient2(low = "purple", high = "green", mid = "white", 
                                                    midpoint = 0,
                                                    name = "Difference from US average (%)") 
# plot 
p2<-ggplot(stateslevel.geo, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = MeanhappeningOppose), color = "black", size = 0.25) + 
  coord_map() + theme_void() + scale_fill_gradient2(low = "purple", high = "green", mid = "white", 
                                                    midpoint = 0,
                                                    name = "Difference from US average (%)") 
# plot 
p3 <- ggplot(stateslevel.geo, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = MeanDNhappening), color = "black", size = 0.25) + 
  coord_map() + theme_void() + scale_fill_gradient2(low = "purple", high = "green", mid = "white", 
                                                    midpoint = 0,
                                                    name = "Difference from US average (%)") 
p1 <- p1 + theme(
  legend.text = element_text(size = 6), # Smaller legend text
  legend.title = element_text(size = 9), # Smaller legend title
  legend.key.size = unit(0.5, "lines"), # Adjusts the size of the legend keys
  legend.spacing = unit(1, "lines") # Adjusts the spacing between legend keys
)
p2 <- p2 + theme(
  legend.text = element_text(size = 6),
  legend.title = element_text(size = 9),
  legend.key.size = unit(0.5, "lines"),
  legend.spacing = unit(0.5, "lines")
)
p3 <- p3 + theme(
  legend.text = element_text(size = 6),
  legend.title = element_text(size = 9),
  legend.key.size = unit(0.5, "lines"),
  legend.spacing = unit(0.5, "lines")
)

# Combine plots with unified legend settings
combined_plot <- (p1 + p2 + p3) +
  plot_layout(nrow = 3, guides = "collect") + 
  plot_annotation(
    tag_levels = 'A',
    title = "Relative Geographical Variation in Climate Awareness, Uawareness and Denial",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 10), # Adjust main title size
      plot.tag = element_text(size = 5) # Adjust subplot label size
    )
  )

# Display the combined plot
combined_plot
##Code for Box Plot
countieslevel$DNhappening <- 100 - (countieslevel$happening + countieslevel$happeningOppose)
countieslevel$MeanDNhappening <-  (countieslevel$DNhappening - (100 - (70.151 + 12.427)))
# Create a combined dataset with a grouping factor
combined_data <- data.frame(
  values = c(stateslevel$DNhappening, countieslevel$DNhappening),
  group = factor(rep(c("Counties", "States"), c(length(stateslevel$DNhappening), length(countieslevel$DNhappening))))
)
# Create the boxplot with different colors
boxplot(values ~ group, data = combined_data, col = c("blue", "red"),
        main = "Variation in % of Climate Unaware People",
        ylab = "% of people",
        names = c("State Level", "County Level"))
#Code for Scatter Plot
ggplot(countieslevel, aes(x = DNhappening, y = happeningOppose)) +
  geom_point(pch = 1) +  # Using open circles for points
  labs(
    x = "% Unaware of Global Warming",
    y = "% Denying Global Warming",
    title = "Global Warming: Awareness Versus Denial at the County Level"
  ) +
  theme_minimal() +  # Using a minimal theme for aesthetics
  theme(
    plot.title = element_text(size = 11)  # Adjust title size
  )
# Calculate the Pearson correlation coefficient
correlation_coefficient <- cor(countieslevel$DNhappening, countieslevel$happeningOppose)
# Square the Pearson correlation coefficient to get R^2
r_squared <- correlation_coefficient^2
# Print the R^2 value
print(r_squared)

