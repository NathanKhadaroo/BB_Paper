# Load library: -----------------------------------------------------------

library(rsample)
library(tidyverse)
library(patchwork)



#conflicted::conflict_prefer("View", "utils")
#conflicted::conflict_prefer("filter", "dplyr")

# Load data: --------------------------------------------------------------

data <- read_csv(here::here("Data",
                            "base_file_all_final.csv"))

df <- data %>% 
  filter(version == "v2016") %>%
  group_by(user_id) %>%
  count() %>%
  ungroup() %>% 
  mutate(engagement_threshold = if_else(n > 1,
                                        "high", "low")) %>% 
  right_join(data %>% 
               filter(version == "v2016"), by = "user_id")
  

# Bootstrapped version: ---------------------------------------------------
# Creating 10000 bootstraped data sets:

df_high <- df %>% filter(engagement_threshold == "high")

df_low <- df %>% filter(engagement_threshold == "low")

bootstrap_high <- bootstraps(df_high, times = 5000)

bootstrap_low <- bootstraps(df_low, times = 5000)


# How I'm doing -----------------------------------------------------------

#Mean calculating function

fun <- function(splits) {
 mean(analysis(splits)$how_im_doing)
     }

#Creating a df of bootstrapped means

test <- bootstrap_high %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            group = "high")

test2 <- bootstrap_low %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            group = "low")


means <- test %>% 
  rbind(test2)



p1 <- ggplot(means, aes(avg, fill = group)) + 
  geom_histogram(alpha = 0.5,
                 bins = 70,
                 aes(y = ..density..),
                 position = 'identity')+
  theme_minimal()


# Nose --------------------------------------------------------------------

#Mean calculating function

fun <- function(splits) {
  mean(analysis(splits)$nose)
}

#Creating a df of bootstrapped means

test <- bootstrap_high %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            group = "high")

test2 <- bootstrap_low %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            group = "low")


means <- test %>% 
  rbind(test2)



p2 <- ggplot(means, aes(avg, fill = group)) + 
  geom_histogram(alpha = 0.5,
                 bins = 70,
                 aes(y = ..density..),
                 position = 'identity')+
  theme_minimal()

# Eyes --------------------------------------------------------------------

#Mean calculating function

fun <- function(splits) {
  mean(analysis(splits)$eyes)
}

#Creating a df of bootstrapped means

test <- bootstrap_high %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            group = "high")

test2 <- bootstrap_low %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            group = "low")


means <- test %>% 
  rbind(test2)



p3 <- ggplot(means, aes(avg, fill = group)) + 
  geom_histogram(alpha = 0.5,
                 bins = 70,
                 aes(y = ..density..),
                 position = 'identity')+
  theme_minimal()

# Breathing --------------------------------------------------------------------

#Mean calculating function

fun <- function(splits) {
  mean(analysis(splits)$breathing)
}

#Creating a df of bootstrapped means

test <- bootstrap_high %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            group = "high")

test2 <- bootstrap_low %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            group = "low")


means <- test %>% 
  rbind(test2)



p4 <- ggplot(means, aes(avg, fill = group)) + 
  geom_histogram(alpha = 0.5,
                 bins = 70,
                 aes(y = ..density..),
                 position = 'identity')+
  theme_minimal()


p1|p2|p3|p4




# Is higher variance for smaller groups just mechanically the case --------

#Making some fake data

#normal

apples <- data.frame(size = rnorm(100000, 5, 2))

oranges <- data.frame(size = rnorm(5000, 5, 2))

#uniform (same problem)

#apples <- data.frame(size = runif(100000, min = 0, max = 5))
#
#oranges <- data.frame(size = runif(5000, min = 0, max = 5))


apples$fruit <- 'apple'

oranges$fruit <- 'orange'



#Bootstrapping


bootstrap_apples <- bootstraps(apples , times = 1000)

bootstrap_oranges <- bootstraps(oranges, times = 1000)


#Mean calculating function

fun <- function(splits) {
  mean(analysis(splits)$size)
}

#Creating a df of bootstrapped means

test_apples <- bootstrap_apples %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            fruit = "apple")

test_oranges <- bootstrap_oranges %>% 
  transmute(avg = map(splits, fun),
            avg = as.numeric(avg),
            fruit = "orange")


means <- test_apples %>% 
  rbind(test_oranges)



ggplot(means, aes(avg, fill = fruit)) + 
  geom_histogram(alpha = 0.5,
                 bins = 70,
                 aes(y = ..density..),
                 position = 'identity')+
  theme_minimal()

# Yes

