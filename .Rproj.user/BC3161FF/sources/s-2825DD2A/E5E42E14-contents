

conflicted::conflict_prefer("View", "utils")
df <- read_csv(here::here("Data",
                          "base_file_all_final.csv"))



lubridate::year(date) == 2018)

df %>%
  mutate(month = lubridate::month(date)) %>% 
  select(month)
  group_by(lubridate::year(date))%>%
  summarise(freq=n())


 df %>% 
  dplyr::filter(lubridate::year(date) == 2021) %>%
   arrange(desc(date)) %>% summarise(date)
 
 %>%
  group_by(user_id) %>% 
  distinct(date)%>%
  do(sequence = as.numeric(seq.Date(min(.$date),
                                    as.Date('2021-12-31'),
                                    by = '1 day') %in% .$date) + 1)
