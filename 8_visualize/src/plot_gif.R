library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)
library(sf)
# find a highly observed year
obs <- readRDS('2_observations/out/obs_temp_drb.rds')

year_obs <- obs %>%
  group_by(year = lubridate::year(date)) %>%
  summarize(n_obs = n(), 
            n_sites = length(unique(seg_id_nat)))

# year 2018 has the most records, let's start there!
obs_2018 <- filter(ungroup(obs), lubridate::year(date) == 2018) %>%
  select(subseg_id, date)

# arrange by site and year, collapse to every three days
# TRUE/FALSE whether it was observed
 date_lookup <- tibble(date = seq.Date(from = as.Date('2018-01-01'), to = as.Date('2018-12-31'), by = 1),
                           group = c(rep(seq(1, 361, by = 3), each = 3), 364, 364))
# 
#obs_2018 <- left_join(obs_2018, date_lookup)

site_obs <- obs_2018 %>%
  left_join(date_lookup) %>%
  mutate(id = paste(group, subseg_id, sep = '_')) %>%
  mutate(obs = TRUE)


site_obs_plus <- site_obs %>%
  mutate(group = group+3) %>%
  filter(group <= max(site_obs$group)) %>%
  mutate(id = paste(group, subseg_id, sep = '_')) %>%
  filter(!id %in% unique(site_obs$id))

site_obs_minus <- site_obs %>%
  mutate(group = group-3) %>%
  filter(group >- min(site_obs$group)) %>%
  mutate(id = paste(group, subseg_id, sep = '_')) %>%
  filter(!id %in% unique(site_obs$id))

site_obs_plus_minus <- select(site_obs_plus, subseg_id, group) %>%
  bind_rows(select(site_obs_minus, subseg_id, group)) %>%
  distinct() %>%
  mutate(obs = FALSE)

head(site_obs)

# read in network for all possible reach IDs
network <- readRDS('1_network/out/network.rds')
all_sites <- unique(network$edges$subseg_id)

 
black_theme <- theme(
  # get rid of panel grids
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change plot and panel background
  plot.background=element_rect(fill = "black", color = 'black'),
  panel.background = element_rect(fill = 'black', color = 'black'),
  # Change legend 
  legend.position = c(0.6, 0.07),
  legend.direction = "horizontal",
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect(color = "black", fill = "black"),
  legend.title = element_text(color = "white"),
  legend.text = element_text(color = "white"),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank(),
  plot.caption = element_text(color = 'grey30', size = 20, hjust = 0.08, vjust = 4)
)

date_lookup2 <- rename(date_lookup, plot_date = date) %>%
  group_by(group) %>%
  summarise_all(funs(first(.)))
  
  
c_obs <- bind_rows(site_obs, site_obs_plus_minus) %>%
  arrange(subseg_id, date, group) %>%
  group_by(subseg_id, group) %>%
  summarise_all(funs(first(.)))%>%
  ungroup() %>%
  left_join(date_lookup2)


plot_obs <- network2 %>%
  right_join(c_obs)



p <- ggplot(network2) +
  geom_sf(color = 'grey16', size = 0.3) +
  theme_classic()+
  black_theme

temp_dates <- as.Date('2018-01-01')+ 0:2
temp <- filter(plot_obs, date %in% temp_dates) %>%
  arrange(date) %>%
  mutate(date2 = format(date, '%b %d'))
temp <- plot_obs %>%
  arrange(date) %>%
  mutate(date2 = format(date, '%b %d'))
  
temp <- plot_obs %>%
  arrange(group)

p2 <- p +
  #observed line
  geom_sf(data = filter(temp, obs), color = 'lightblue', size = 0.1) +
  # delay observed line
  geom_sf(data = filter(temp, !obs), color = alpha('lightblue', 0.3), size = 0.1) +
  #observed cloud
  geom_sf(data = filter(temp, obs), color = alpha('lightblue', 0.15), size = 3) +
  geom_sf(data = filter(temp, obs), color = alpha('lightblue', 0.45), size = 1.5) +
  # delaye observed cloud
  geom_sf(data = filter(temp, !obs), color = alpha('lightblue', 0.15), size = 1.5) +
  gganimate::transition_states(group, transition_length = 0) +
  labs(caption = 'Day of year: {closest_state}')
gganimate::animate(p2, width = 300, height = 300, bg = 'transparent')

gganimate::animate(p2, width = 1000, height = 1000, bg = 'transparent', nframes = 125)
anim_save('obs_network_2018_simplified.gif')

# black just the network, identical but with no data, reaches = white
# every reach that is observed
# put the ticker in
# keep it to < 30 mb
network2 <- sf::st_simplify(network$edges, dTolerance = 1000)
p3 <- ggplot(network2) +
  geom_sf(color = 'lightblue', size = 0.3) +
  theme_classic()+
  black_theme +
  labs(caption = 'Delaware River Basin') +
  theme(plot.caption = element_text(color = 'grey30', size = 12, hjust = 0.08, vjust = 4))

ggsave('blank_network2.png', p3, height = 7.5, width = 10.33, bg = 'transparent')
  

# create a bunch of pngs

make_plots <- function(this_group) {
  
  temp <- filter(plot_obs, group %in% this_group)
  this_date <- unique(temp$plot_date)
  my_caption <- format(this_date, "%b %d")
  p2 <- p +
    #observed line
    geom_sf(data = filter(temp, obs), color = 'lightblue', size = 0.1) +
    # delay observed line
    geom_sf(data = filter(temp, !obs), color = alpha('lightblue', 0.3), size = 0.1) +
    #observed cloud
    geom_sf(data = filter(temp, obs), color = alpha('lightblue', 0.15), size = 3) +
    geom_sf(data = filter(temp, obs), color = alpha('lightblue', 0.45), size = 1.5) +
    # delaye observed cloud
    geom_sf(data = filter(temp, !obs), color = alpha('lightblue', 0.15), size = 1.5) +
    #gganimate::transition_states(date, transition_length = 0) +
    labs(caption = my_caption)
  
  
  print(paste0("saving plot ", this_date))
  ggsave(filename = paste0("gif_out_red/",format(this_date, "%Y%m%d"),".png"),
         width = 13.33, height = 7.5, dpi = 150, bg = 'transparent')
}
library(purrr)
library(magick)
library(dplyr)

seq(1, 122, by=1) %>% 
  map_df(make_plots)

# Step 2: List those Plots, Read them in, and then make animation
list.files(path = "gif_out_red", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=5) %>% # animates, can opt for number of loops
  image_write("gif_out_red/obs_network_2018.gif") # write to current dir
