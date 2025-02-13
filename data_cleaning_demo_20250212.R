# This script demonstrates multiple techniques for cleaning a dataset in R, including handling missing data,  
# removing outliers, splitting cells, correcting typos, and more.  
# The example dataset represents salmon catch data obtained from the Alaska Department of Fish & Game,  
# covering fish catch records from 1878 to 1997.  
# The original dataset is available at: https://knb.ecoinformatics.org/view/df35b.304.2  
# Data credit: Mike Byerly (2016). "Alaska Commercial Salmon Catches by Management Region (1886-1997)."  
# Gulf of Alaska Data Portal. df35b.304.2.  
# The original data has been modified by introducing various errors, artificial variables,  
# and additional artifical data to demonstrate cleaning techniques.  
#
#Code created by Carlos Ramirez-Reyes cramirezreyes@unr.edu
#Last updated Feb 12 2015
#

##################DA
# Load necessary libraries ####
library(tidyverse)  # Collection of R packages for data manipulation and visualization
library(janitor) #Load janitor for data cleaning functions
library(stringdist) #Finds similar text patterns
library(naniar)  #Visualize missing data

# Read in the original and additional datasets
catch1 <- read_csv("data/catch_original.csv", na = c("", "NA"))  # Treats empty strings and "NA" as missing values (both exist in the data)
catch2 <- read_csv("data/catch_additional.csv")

# Preview the first few rows to understand the structure
head(catch1)
head(catch2)

# Skip the first two rows since they contain metadata or headers we don't need
catch1 <- read_csv("data/catch_original.csv", skip = 2, na = c("", "NA"))
catch2 <- read_csv("data/catch_additional.csv", na = c("", "NA")) #not skipping anything since it doesn't have conflicts
head(catch1)
head(catch2)

# Combine both datasets ####
catch <- bind_rows(catch1, catch2)
colnames(catch)  

#Clean column names ####
# Clean column names to be consistent and easier to reference during coding
catch <- catch %>%
  clean_names()
colnames(catch)  



#Remove duplicate records ####
# Identify and remove duplicated records
nrow(catch)  # Total number of records
nrow(catch %>%
       distinct())  # Number of unique records

# Remove duplicates
catch <- catch %>%
  distinct()
nrow(catch)

#Missing data####
# Count missing values in each column
sapply(catch, function(x) sum(is.na(x)))  #across all columns list the # of NA
#represent it as percentage
catch %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100))

# Visualize missing data patterns using library naniar
vis_miss(catch)

#Remove missing data with any missing value: Least recommended!!!!! 
catch_no_na <- catch %>%
    drop_na()  # Removes rows with any missing values
dim(catch_no_na)
#Remove Rows with Missing Values in Specific Columns:
catch_no_na <- catch %>%
  drop_na(sockeye_salmon, region) #only removed if data is missing in either of those two columns
dim(catch_no_na)

#Recommended, use option  na.rm = TRUE in individual calculations
#For instance, whats the mean sockeye_salmon catch across years
mean(catch$sockeye_salmon)  #Should produce NA, since NAs exist in the column
sum(is.na(catch$sockeye_salmon))  #count NAs. Yup, we have 50 instances

mean(catch$sockeye_salmon, na.rm = TRUE)   #This should work, NAs were ignored

#Imputing data: Replace NA with the mean of sockeye_salmon
catch_input <- catch %>%
  mutate(sockeye_salmon = if_else(is.na(sockeye_salmon), 
                                  mean(sockeye_salmon, na.rm = TRUE), 
                                  sockeye_salmon))
sum(is.na(catch_input$sockeye_salmon))  # Before we had 50 missing records, now is zero
# library(VIM)  for more advanced inputation techniques
# catch_knn <- kNN(catch, variable = "sockeye_salmon", k = 5)

#End of missing data part of the demo
#Before proceeding, let's do a couple of processes on the data 
#Remove unnecessary columns####
catch <- catch %>%
  select(-c(notes, entered_by))

#let's add total salmon count and a unique observation ID to the dataframe
#note we're removing NA's from calculation
catch <- catch %>%
  mutate(
    total_salmon = rowSums(across(chinook_salmon:chum_salmon), na.rm = TRUE),
    observation_id = row_number()
  )


#Outliers #####
# Identify outliers in the sockeye_salmon column
colnames(catch)  # Check available columns
plot(catch$year, catch$sockeye_salmon)  # Visualize potential outliers on this column
boxplot(catch$sockeye_salmon)  # Visualize potential outliers on this column
ggplot(catch, aes(x = sockeye_salmon)) + 
  geom_histogram(bins = 30) + 
  ggtitle("Distribution of Sockeye Salmon (Before Cleaning)")


#Method 1 to remove outliers: Interquartile Range (IQR) 
# Calculate the Interquartile Range (IQR) for formal outlier detection
Q1 <- quantile(catch$sockeye_salmon, 0.25, na.rm = TRUE)
Q3 <- quantile(catch$sockeye_salmon, 0.75, na.rm = TRUE)
IQR_value <- IQR(catch$sockeye_salmon, na.rm = TRUE)
# Define outlier thresholds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
# Filter out outliers
outliers <- catch %>%
  filter(sockeye_salmon < lower_bound | sockeye_salmon > upper_bound) %>% 
  arrange(sockeye_salmon)
# Print identified outliers
print(outliers)

#Method 2 to remove outliers
# Remove extreme values based on domain knowledge
min_realistic <- 0   #We know we can't have negative catch per year
max_realistic <- 50000  # Hypothetical maximum. In other systems could be
                        #max temp; max height of trees, max number of sits in room, etc

#Find extreme values into an object
extreme_values <- catch %>%
  filter(sockeye_salmon < min_realistic | sockeye_salmon > max_realistic)

#Now remove those extreme records from the dataset
catch <- anti_join(catch, extreme_values)

# Verify data after removing outliers
plot(catch$year, catch$sockeye_salmon)
boxplot(catch$sockeye_salmon)  
boxplot(catch$total_salmon)  
# We are now able to make summaries on data, such as calculating the mean catch per region
catch %>% 
  group_by(region) %>% 
  summarize(mean_catch = mean(total_salmon))
#What about per source of fish data
catch %>% 
  group_by(source) %>% 
  summarize(mean_catch = mean(total_salmon))

#There is a problem on this column 
unique(catch$source)

#Let's clean the column in steps
#Split columns####
# Step 1. Split the 'source' column into multiple rows using the ";" delimiter
catch_long <- catch %>% separate_longer_delim(source, delim = ";")
unique(catch_long$source)
#Step2. 
#Standardize source values to lowercase ####
catch_long$source <- tolower(catch_long$source)
unique(catch_long$source)

#Step 3 Manually correct typos####
catch_long$source <- gsub("\\b(rgion|region)\\b", "regional", catch_long$source)
catch_long$source <- gsub("\\b(locals|loccal)\\b", "local", catch_long$source)
catch_long$source <- gsub("\\b(individuals|individuall|indivifual|individ)\\b", "individual", catch_long$source)
unique(catch_long$source)
#The \\b at both ends of the pattern ensures that only whole words are matched 
#(word boundaries), so partial replacements won't happen.

#Automatically correct typos####
#An automatic way is to match source names to correct terms using string distance
#Step 1. Let's replicate catch_long object
catch_long <- catch %>% separate_longer_delim(source, delim = ";")
unique(catch_long$source)
#Step2. Standardize source values to lowercase
catch_long$source <- tolower(catch_long$source)
unique(catch_long$source)

#Step 3 use the library(stringdist) 
#Provide reference names
correct_names <- c("individual", "local", "regional")  #You should give these
#Now run amatch to find closest records to the references
catch_long$source_cor <- correct_names[amatch(catch_long$source, correct_names, maxDist = 3)]
unique(catch_long$source_cor)

#Data is clean! 


# Summarize mean catch by corrected source
catch_long %>% 
  group_by(source_cor) %>% 
  summarise(mean_catch = mean(sockeye_salmon, na.rm = TRUE), instances = n())

# Conduct t-test to compare local and regional sources
t.test(sockeye_salmon ~ source_cor, data = catch_long, subset = source_cor %in% c("local", "regional"))

#Pivoting data
#Sometimes you want to have data in wide format, like when you want enter additional data or combining with other datasets
catch_wide <- catch_long %>% 
  select(-source) %>% #Necessary to remove the original source column o prevent conflict with new, clean source_cor
  #group_by(observation_id) %>% 
  mutate(source_log = TRUE) %>%  #Adding "TRUE" across all observations
  pivot_wider(names_from = source_cor,  #Local, individual, regional will have each a column
              values_from = source_log, #Use TRUE to populate these new columns if any of the three source categories is exist for the observation
              values_fill = list(source_log = FALSE)) #If a source wasn't reported, then add a FALSE instead

view(catch_wide)  #now, all observations are collapsed back to one per row

#Part two
#Let's add now full names for each region that are listed in another dataset

region_defs <- read_csv("data/region_defs.csv")
head(region_defs)
#same issue, additional rows we don't need
region_defs <- read_csv("data/region_defs.csv", skip = 2, na = c("", "NA"))
colnames(region_defs)
head(region_defs)

#Clean the names to remove capitals and spaces if any
region_defs <- region_defs %>%
  clean_names()
colnames(region_defs)
head(region_defs)

#Something interesting with column coordinates, it needs to be split. 
#Split column into two####
# Split coordinates and convert to numeric
# Step 1: Split into two columns
region_defs <- region_defs %>%
  separate(coordinates, into = c("lon", "lat"), sep = ",")

#Step 2, remove the letters "Lon, Lat"
region_defs$lon <- gsub("Lon", "", region_defs$lon)
region_defs$lat <- gsub("Lat", "", region_defs$lat)
#Step 3, convert it to numeric data
region_defs$lon <- as.numeric(region_defs$lon)
region_defs$lat <- as.numeric(region_defs$lat)


#Combine both datasets by common values in column, note that region_defs column 
#is named code, and catch_long is named region. Keep only values in catch_long
combined_data <- catch_long %>%
  left_join(region_defs, by = c("region" = "code"))

colnames(combined_data)
#Remove data columns we don't need
combined_data <- combined_data %>% 
  select(-c(region_code, notes))



# Summarize mean catch by region
combined_data %>% 
  group_by(mgmt_area) %>% 
  summarise(mean_catch = mean(sockeye_salmon, na.rm = TRUE),  # Adding na.rm = TRUE for safety
            instances = n())

#####We have clean data!!!!!!!!!
#Let's save it to a file
write_csv(combined_data, "data/catch_clean_20250212.csv")

#Bonus, let's visualize data in space
#Use sf to convert combined data as spatial points using lon lat columns and then plot them in ggplot. 
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# 1 Convert dataframe to Spatial Points using the coordinates columns and the sf library
combined_sf <- combined_data %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

ggplot() +
  geom_sf(data = combined_sf, color = "steelblue", alpha = 0.7) 
#Good, we have spatial points, but we can add insights about it  

# 2 Calculate Mean Catch per Region
mean_catch_data <- combined_data %>%
  group_by(region) %>%
  summarise(mean_catch = mean(total_salmon, na.rm = TRUE),
            lon = first(lon),  #Take first coordinate in the region to be used in column 
            lat = first(lat))   #Same for lon (longitude column)

view(mean_catch_data)  

#Now that we have this summary table, let's use the coordinates columns to convert it to spatial object
mean_catch_data <-  st_as_sf(mean_catch_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE) # make the object a spatial object
  
# 3 Plotting it in ggplot and using column mean_catch to represent the size of points in map
ggplot() +
  geom_sf(data = mean_catch_data, aes(size = mean_catch), color = "steelblue", alpha = 0.7) +
  scale_size_continuous(name = "Mean Catch") +
  labs(title = "Mean Salmon Catch by Region in tons", x = "Longitude", y = "Latitude")+
  theme_minimal()


#Even better, add some reference maps to be pulled from the library(rnaturalearth) and library(rnaturalearthdata)
# Get basemap data for USA and Canada
north_america <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(admin %in% c("United States of America", "Canada"))

ggplot() +
  geom_sf(data = north_america, fill = "gray", color = "black")  # Basemap for Alaska & Canada
#Notice that map stretches to all world longitudes, but we can still use it, see below
 
# Plot both the north_america map and the mean_catch_data in the same map
ggplot() +
  geom_sf(data = north_america, fill = "gray80", color = "white") +  # Basemap for Alaska & Canada
  geom_sf(data = mean_catch_data, aes(size = mean_catch), color = "steelblue", alpha = 0.7) +
  scale_size_continuous(name = "Mean Catch") +
  coord_sf(xlim = c(-170, -110), ylim = c(50, 72)) +  # Zoom into Alaska & Western Canada 
  labs(title = "Mean Salmon Catch by Region",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

##End of file##