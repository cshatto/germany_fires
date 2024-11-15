library(terra)
library(tidyverse)
library(RColorBrewer)
# library(exactextractr)
setwd('Projects/Fires_in_Germany/')

#filter europe shp down to germany & save
germany <- vect('Data/Germany_Outline/DEU_Boundaries/DEU_adm1.shp')
plot(germany)
#use germany shp to filter effis fire observations to germany & save
# allFires <- vect('Data/Fire_Data/EO/effis_layer/modis.ba.poly.shp')
# allFires <- project(allFires, germany)
# deFires <- intersect(allFires,germany)
# plot(germany)
# lines(deFires)
# writeVector(deFires, 'Fires-in-Germany/Fire_Data/germany_fires/effis_germany.shp',overwrite=T)

#reload germany fires and wrangle only the date, burned area, and state info
deFires <- vect('Data/Fire_Data/EO/germany_fires/effis_germany.shp')
deFires_df <- as.data.frame(deFires)
deFires_BA <- deFires_df[,c(2,7,23)]
deFires_BA$FIREDATE <- as.numeric(substr(deFires_BA$FIREDATE,1,4))

#reported data only goes back to 2010
deFires_BA <- deFires_BA %>% 
  filter(FIREDATE > 2009)
deFires_BA <- deFires_BA %>%
  rename(YEAR = FIREDATE)

#fill other variables
deFires_BA$TYPE <- "Earth Observation"
deFires_BA$CAUSE <- "Unknown"

#load reported fire data
report <- read.csv('Data/Fire_Data/Reported/waldbrandstatistik.csv',
                   fileEncoding = "UTF-16LE",sep=';')
report$Einheit <- as.factor(report$Einheit)
report$Wert <- as.numeric(report$Wert)
# Filter out rows with "€" or "m³" in the 'Einheit' column
report <- report %>%
  filter(!(Einheit %in% c("€", "m3")))

str(report)
report_fix <- report %>%
  pivot_wider(
    names_from = colnames(report)[6],  # Column 6 becomes column headers
    values_from = colnames(report)[8]  # Column 8 fills the new columns
  )
report_fix$YEAR <- substr(report_fix$Zeit, 1, 4)
# Format the date to year-month
# report_fix$FIREDATE <- format(report_fix$FIREDATE, "%Y-%m")
report_fix$TYPE <- "Reported"
report_fix$AREA_HA <- report_fix$ha
report_BA <- report_fix[,c(9:11,3,4)]


#merge datasets
names(deFires_BA)[names(deFires_BA) == "NAME_1"] <- "STATE"
names(report_BA)[names(report_BA) == "Gebiet"] <- "STATE"
names(deFires_BA)[names(deFires_BA) == "Waldbrände"] <- "CAUSE"
names(report_BA)[names(report_BA) == "Waldbrände"] <- "CAUSE"

allFires <- rbind(deFires_BA,report_BA)
# allFires$FIREDATE <- as.Date(paste0(substr(allFires$FIREDATE, 1, 4), "-", substr(allFires$FIREDATE, 5, 6), "-01"), "%Y-%m-%d")
# allFires$FIREDATE <- as.Date(allFires$FIREDATE)
allFires$AREA_HA <- as.numeric(allFires$AREA_HA)
allFires$STATE <- as.factor(allFires$STATE)
allFires$TYPE <- as.factor(allFires$TYPE)
allFires$YEAR <- as.numeric(allFires$YEAR)
str(allFires)
allFires <- allFires %>% 
  filter(YEAR < 2024 & YEAR > 2012) %>% 
  filter(STATE != "Deutschland")

# Create the line plot using ggplot2
ggplot(allFires, aes(x = YEAR, y = AREA_HA, fill = TYPE, group=TYPE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Burned Area (Ha)", fill= "Fire Record Type") +
  scale_x_continuous(breaks = seq(min(allFires$YEAR), max(allFires$YEAR), by = 1)) +  # Show every year
  theme_minimal(base_size = 12) +  # Set the base font size to 12 pt
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # x-axis labels
    axis.text.y = element_text(size = 12),  # y-axis labels
    axis.title.x = element_text(size = 12),  # x-axis title
    axis.title.y = element_text(size = 12),  # y-axis title
    legend.text = element_text(size = 12),  # Legend text
    legend.title = element_text(size = 12),  # Legend title
    plot.title = element_text(size = 12)  # Plot title if applicable
  ) +   
  scale_fill_manual(values = c("Earth Observation" = "orange", "Reported" = "grey40"))
# ggsave("Fires-in-Germany/Outputs/EOvsReported_barchart.png", width = 8, height = 6, dpi = 300)

# Create the line plot using ggplot2
ggplot(allFires, aes(x = CAUSE, y = AREA_HA, fill = TYPE, group=TYPE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Cause", y = "Burned Area (Ha)", fill= "Fire Record Type") +
  # scale_x_continuous(breaks = seq(min(allFires$YEAR), max(allFires$YEAR), by = 1)) +  # Show every year
  theme_minimal(base_size = 12) +  # Set the base font size to 12 pt
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),  # x-axis labels
    axis.text.y = element_text(size = 9),  # y-axis labels
    axis.title.x = element_text(size = 12),  # x-axis title
    axis.title.y = element_text(size = 12),  # y-axis title
    legend.text = element_text(size = 12),  # Legend text
    legend.title = element_text(size = 12),  # Legend title
    plot.title = element_text(size = 12)  # Plot title if applicable
  ) +   
  scale_fill_manual(values = c("Earth Observation" = "orange", "Reported" = "grey40"))
# ggsave("Fires-in-Germany/Outputs/EOvsReported_Waldbrand_barchart.png", width = 8, height = 6, dpi = 300)



#load in forest data
forests <- rast('Data/Forest_Types_Blickensdorfer/Dominant_Species_Class.tif')
spp_codes <- read.csv('Data/Forest_Types_Blickensdorfer/tree_species_code.csv')

spp_codes <- c(
  "2" = "Birch",
  "3" = "Beech",
  "4" = "Douglas Fir",
  "5" = "Oak",
  "6" = "Alder",
  "8" = "Spruce",
  "9" = "Pine",
  "10" = "Larch",
  "14" = "Fir",
  "16" = "ODH",
  "17" = "ODL",
  "Non-forested" = "Non-forested"
)

germany <- project(germany,forests)
germany <- makeValid(germany)
deFires <- makeValid(deFires)
names(deFires)[names(deFires) == "NAME_1"] <- "STATE"
deFires_shp <- sf::st_as_sf(deFires)

extracted_values <- terra::extract(forests, deFires, fun = NULL, na.rm = TRUE, weights = TRUE,ID=T)
extracted_values$Dominant_Species_Class[is.na(extracted_values$Dominant_Species_Class)] <- "Non-forested"

percent_cover_list <- lapply(split(extracted_values, extracted_values$ID), function(df) {
  total_area <- sum(df$weight)
  percent_cover <- aggregate(weight ~ Dominant_Species_Class, data = df, FUN = function(x) sum(x) / total_area * 100)
  return(percent_cover)
})

# Combine the results into a data frame with polygon IDs
result <- do.call(rbind, lapply(seq_along(percent_cover_list), function(i) {
  data.frame(ID = names(percent_cover_list)[i], percent_cover_list[[i]])
}))
names(result)[names(result) == "Dominant_Species_Class"] <- "Dom_Species"

result <- result %>%
  mutate(Dom_Species = spp_codes[Dom_Species])
deFires$ID <- 1:length(deFires)
deFires_df <- as.data.frame(deFires)
final_result <- merge(deFires_df, result, by = "ID", all.x = TRUE)
final_result <- final_result[,c(1,3,4,8,24,29,30)]

names(final_result)[names(final_result) == "weight"] <- "PERCENT_COVERAGE"
names(final_result)[names(final_result) == "Dom_Species"] <- "DOM_SPECIES"

final_result$FIREDATE <- as.Date(final_result$FIREDATE)
final_result$LASTUPDATE <- as.Date(final_result$LASTUPDATE)
final_result$AREA_HA <- as.numeric(final_result$AREA_HA)
final_result$AREA_HA <- as.character(final_result$AREA_HA)
final_result$STATE <- as.factor(final_result$STATE)
final_result$DOM_SPECIES <- as.factor(final_result$DOM_SPECIES)

str(final_result)

# Summarize data by STATE and DOM_SPECIES
data_summary <- final_result %>%
  group_by(STATE, DOM_SPECIES) %>%
  summarize(total_percent_coverage = sum(PERCENT_COVERAGE, na.rm = TRUE)) %>%
  ungroup()

# Define the Spectral color palette
palette_colors <- c(brewer.pal(11, "Spectral"), "Non-forested" = "grey75")
names(palette_colors) <- c("Alder", "Beech", "Birch", "Douglas Fir", "Fir", "Larch", "Oak", "ODH", "ODL", "Pine", "Spruce", "Non-forested")
data_summary$DOM_SPECIES <- factor(data_summary$DOM_SPECIES, 
                                   levels = c("Non-forested","Alder", "Beech", "Birch", "Douglas Fir", 
                                              "Fir", "Larch", "Oak", "ODH", "ODL", 
                                              "Pine", "Spruce"))
# Plot the data with STATE on the x-axis
ggplot(data_summary, aes(x = STATE, y = total_percent_coverage, fill = DOM_SPECIES)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = palette_colors) +
  labs(
    x = "State",
    y = "Total Percent Burned Area",
    fill = "Dominant Species"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5)  # Center-align title
  )


fire_counts <- final_result %>%
  group_by(STATE, DOM_SPECIES) %>%
  summarize(fire_occurrences = n()) %>%
  ungroup()

# Reorder DOM_SPECIES factor levels to place "Non-forested" last
fire_counts$DOM_SPECIES <- factor(fire_counts$DOM_SPECIES, 
                                  levels = c("Alder", "Beech", "Birch", "Douglas Fir", 
                                             "Fir", "Larch", "Oak", "ODH", "ODL", 
                                             "Pine", "Spruce", "Non-forested"))

# Define the Spectral color palette
palette_colors <- c(brewer.pal(11, "Spectral"), "Non-forested" = "grey75")
names(palette_colors) <- c("Alder", "Beech", "Birch", "Douglas Fir", "Fir", "Larch", "Oak", "ODH", "ODL", "Pine", "Spruce", "Non-forested")
fire_counts$DOM_SPECIES <- factor(data_summary$DOM_SPECIES, 
                                   levels = c("Non-forested","Alder", "Beech", "Birch", "Douglas Fir", 
                                              "Fir", "Larch", "Oak", "ODH", "ODL", 
                                              "Pine", "Spruce"))

# Plot the data with STATE on the x-axis
ggplot(fire_counts, aes(x = STATE, y = fire_occurrences, fill = DOM_SPECIES)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = palette_colors) +
  labs(
    title = "Number of Fire Occurrences by Dominant Species and State",
    x = "State",
    y = "Number of Fire Occurrences",
    fill = "Dominant Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5)  # Center-align title
  )


