
# Loading libraries: ------------------------------------------------------

library(tidyverse) # CRAN v1.3.1
library(ggplot2)   # CRAN v3.3.6
library(ggdist)    # CRAN v3.1.1
library(gghalves)
library(ggstatsplot)


# Reading in data: --------------------------------------------------------

df <- read_csv(here::here("Data",
                          "base_file_all_final.csv"))


# Splitting into clusters: ------------------------------------------------


## Year one: --------------------------------------------------------------

# Manually loading the HMM clustering due to runtime:

yr1_k4          <- readRDS(file = here::here("Clusterings", "yr1_k4"))

yr1_k4_clusters <- readRDS(file = here::here("Clusterings", "yr1_k4_clusters" ))

# Threshold clustering

y1clust <- df %>%
  filter(lubridate::year(date) == 2016) %>%
  group_by(user_id) %>%
  count() %>%
  ungroup() %>% 
  mutate(engagement_threshold_1 = if_else(n > 1, "high", "low"),
         engagement_threshold_3 = if_else(n > 3, "high", "low"),
         engagement_threshold_5 = if_else(n > 5, "high", "low"),
         engagement_threshold_10 = if_else(n > 10, "high", "low"),
         engagement_kron = if_else(n > 17, "high", "low"))%>% 
  left_join(yr1_k4_clusters,by = "user_id") %>% 
  dplyr::select(-prob.1,
                -prob.2,
                -prob.3,
                -prob.4,
                -n) %>% 
  left_join( df %>%
               filter(lubridate::year(date) == 2016) ,by = "user_id") %>% 
  mutate(age = 2016- as.numeric(year_of_birth))




# Exploring group differences: --------------------------------------------



## Year one: --------------------------------------------------------------

# Age:
names(y1clust)




ggplot(y1clust, aes(x = engagement_threshold_1, y = age)) + 
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95)
  ) + 
  ggdist::stat_dots(
    side = "left", 
    dotsize = .8, 
    justification = 1.05, 
    binwidth = .3
  ) +
  coord_cartesian(xlim = c(1.2, NA))




p1 <- y1clust %>%
  ggplot(aes(age, engagement_threshold_1, color = engagement_threshold_1)) +
  stat_summary(
    geom = "linerange",
    fun.min = function(x) -Inf,
    fun.max = function(x) median(x, na.rm = TRUE),
    linetype = "dotted",
    orientation = "y",
    size = .7
  ) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  geom_point(
    ## draw horizontal lines instead of points
    shape = 95,
    size = 10,
    alpha = .2
  ) 


y1clust %>%
  ggplot(aes(engagement_threshold_1, age, colour = engagement_threshold_1)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .5, )+
  theme_bw()


y1clust %>%
  ggplot(aes(engagement_threshold_1,
             age,
             colour = engagement_threshold_1,
             fill = engagement_threshold_1)) + 
  ggdist::stat_halfeye(data =y1clust, adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA, fill = NA) +
  gghalves::geom_half_point(side = "l", range_scale = 0, shape = 95, size = 15, alpha = .035)+
  theme_bw()

y1clust %>%
  ggbetweenstats(engagement_threshold_1,
                 age)


# Alternative using ggstatsplot -------------------------------------------

## for reproducibility
set.seed(123)
library(ggstatsplot)

## parametric t-test and box plot
p1 <- ggbetweenstats(
  data = y1clust,
  x = engagement_threshold_1,
  y = age,
  xlab = "Engagement",
  ylab = "Age",
  plot.type = "box",
  type = "p",
  conf.level = 0.99,
  title = "Parametric test",
  package = "ggsci",
  palette = "nrc_npg"
)

## Mann-Whitney U test (nonparametric t) and violin plot
p2 <- ggbetweenstats(
  data = y1clust,
  x = engagement_threshold_1,
  y = age,
  xlab = "Engagement",
  ylab = "Age",
  plot.type = "violin",
  type = "np",
  conf.level = 0.99,
  title = "Non-parametric Test (violin plot)",
  package = "ggsci",
  palette = "uniform_startrek"
)

## robust t-test and boxviolin plot
p3 <- ggbetweenstats(
  data = y1clust,
  x = engagement_threshold_1,
  y = age,
  xlab = "Engagement",
  ylab = "Age",
  plot.type = "boxviolin",
  type = "r",
  conf.level = 0.99,
  title = "Robust Test (box & violin plot)",
  tr = 0.005,
  package = "wesanderson",
  palette = "Royal2",
  k = 3
)

## Bayes Factor for parametric t-test and boxviolin plot
p4 <- ggbetweenstats(
  data = y1clust,
  x = engagement_threshold_1,
  y = age,
  xlab = "Engagement",
  ylab = "Age",
  type = "bayes",
  plot.type = "box",
  title = "Bayesian Test (box plot)",
  package = "ggsci",
  palette = "nrc_npg"
)

## combining the individual plots into a single plot
combine_plots(
  list(p1, p2, p3, p4),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "Comparison of age between one time reporters and others",
    caption = "Source: Britain Breathing"
  )
)

# Gender

y1clust$gender %>%
  table()


summary(y1clust$gender)

p1 <- ggbarstats(
  data = y1clust,
  y = engagement_threshold_1,
  x = gender
)


p2 <- ggbarstats(
  data = y1clust,
  y = engagement_threshold_3,
  x = gender
)

p3 <- ggbarstats(
  data = y1clust,
  y = engagement_threshold_5,
  x = gender
)

p4 <- ggbarstats(
  data = y1clust,
  y = engagement_threshold_10,
  x = gender
)

combine_plots(
  list(p1, p2, p3, p4),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "Comparison of gender between different engagement levels (48% male in general sample)",
    caption = "Source: Britain Breathing"
  )
)


# Urban/rural
  