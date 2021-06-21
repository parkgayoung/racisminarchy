library(magrittr)
library(imager)


# load jpg image

saa_simil<- load.image(here::here("analysis/figures/003-keyword-similar-words.jpg"))

ha_simil <- load.image(here::here("analysis/figures/003-ha-keyword-similar-words.jpg"))


#combine figures from both SAA and HA
imappend(list(saa_simil, ha_simil),"x") %>% plot
