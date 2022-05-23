

# Loading libraries: ------------------------------------------------------

library(tidyverse) # CRAN v1.3.1
library(sf)        # CRAN v1.0-7

# Reading data: -----------------------------------------------------------

# Report data
df <- read_csv(here::here("Data",
                          "base_file_all_final.csv"))

# Defining reports data as a spatial object
df_sf <- df %>%
  st_as_sf(coords = c("longitude", "latitude"),
           agr = "constant",
           crs = "WGS84") %>%
  st_transform(crs = 27700)

# Postcode data

postcodes  <- st_read(here::here("Data","postcode-boundaries.kml")) %>%
  st_transform(crs = 27700)

# Assigning reports to postcode areas: ------------------------------------

df_sf_coded<- df_sf %>% 
  st_join(postcodes, left = FALSE)

# Counting reports by postcode area: --------------------------------------

report_counts <- df_sf_coded %>%
  group_by(Name)%>% 
  count() %>%
  as_tibble()%>%
  right_join(as_tibble(postcodes$Name),
             by= c("Name" = "value")) %>%
  mutate(n = replace_na(n,0))


# Summary statistics: -----------------------------------------------------

report_counts %>%
  summarise(mean = mean(n),
            median = median(n),
            min = min(n),
            max = max(n),
            st_dev = sd(n))


# Visualising the number of reports per area: -----------------------------

report_counts %>%
  ggplot(aes(x = n)) +
  geom_histogram(bins = 1000, col = "grey", size = 0.1)+
  theme_minimal()

+
  scale_y_log10()+
  scale_x_log10()+
  labs(x = "Number of days in the study",
       y = "Number of participants")

