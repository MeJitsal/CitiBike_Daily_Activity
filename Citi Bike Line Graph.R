
#install.packages("tidyverse")
#install.packages("distill")

#load tidyverse and distill
library("tidyverse")
library("distill")

#save and open data
savefile <- "data/MTA_recent_ridership_data_20220127.csv"
d <- read.csv(savefile)
d %>% glimpse() 

#rename columns
new_names <- 
  str_c(
    rep(c('subway', 'bus', 'lirr', 
          'mta', 'access_ride', 'bridge_tunnel'), 
        each = 2), 
    rep(c("total", "change"), 
        times = 6), 
    sep = '_'
  )

colnames(d) <- c('date', new_names)

d <- d %>% 
  mutate( date = as_date(date, format = '%m/%d/%Y') ) %>%
  mutate( mta_total = as.numeric(mta_total) ) %>%
  mutate( across( where(is.character), ~str_replace_all(.x, pattern = '%', replacement = '')) ) %>%
  mutate( across( where(is.character), ~as.numeric(.x)) )


#Change any 'N/A' or other non-integer functions to NA
d %>%
  mutate( observation = row_number() ) %>%
  pivot_longer(
    cols = -c(date, observation),
    names_to = 'variable', 
    values_to = 'value') %>%  
  mutate(
    is_missing = if_else(is.na(value), TRUE, FALSE) #can do =is.na(value)
  ) %>%
  ggplot() +
  geom_raster(
    mapping = aes(
      x = observation,
      y = variable,
      fill = is_missing
    )
  ) +
  scale_fill_manual(
    values = c('black', 'darkorange'), 
    breaks = c(FALSE, TRUE)
  )

#rename columns to remove 'date' and 'change

d <- d %>%
  select( contains(c('date', 'change')) ) %>%
  rename_with(~ str_remove_all(.x, '_change')) %>%
  pivot_longer(
    cols = c('subway', 'bus', 'lirr', 'mta', 'access_ride', 'bridge_tunnel'), #can also do (cols=-date) include all BUT DATE
    names_to = 'transportation_type',
    values_to = 'change'
  ) %>%
  mutate(
    change = change - 100
  ) #subtract 100 to find change

#creating line graph of daily activity change
d %>%
  #select most used transportation
  filter(transportation_type %in% c('bridge_tunnel', 'bus', 'subway', 'lirr', 'mta')) %>% # can also do transportation_type != 'access_ride'
  
  ggplot(aes(x = date, y = change, color = transportation_type)) +
  geom_line()+
  scale_color_manual(
    breaks = c('bridge_tunnel', 'bus', 'subway', 'lirr', 'mta'),
    values = c('#367C9D', '#61A0CA', '#91BBF9', '#993865', '#773452') #assign colors to each type of transportation
  ) +
  #name x and y axes
  labs(
    x = 'Date',
    y = 'Percent decline from 2019 ridership'
  )
