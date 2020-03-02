# a non-ggplot way to create gif

library(magick)
library(dplyr)
library(purrr)

list.files(path = "gif_out", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=10) %>% # animates, can opt for number of loops
  image_write("gif_out/obs_network_2018.gif") # write to current dir