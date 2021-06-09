library(tidyverse)
library(quanteda)
library(quanteda.textstats)

if(!exists("ha_abstracts")){
  ha_abstracts <- read_csv(here::here("analysis","data", "ha_titles_abstracts_years_df.csv"))
}

names_ha_text <- names(ha_abstracts)

# count all words for each year
if(!exists("all_text_c_ha")){
  all_text_c_ha <- corpus(all_text_c_ha)
}

# don't remove any stopwords
if(!exists("all_text_c_ha_dtm")){
  all_text_c_dtm <-
    dfm(all_text_c,
        tolower = TRUE,
        verbose = TRUE,
        remove_numbers = TRUE,
        remove_symbols = TRUE,
        split_hyphens = TRUE,
        remove_punct = TRUE)

}

# Explore key words over time
keywords <-
  c("race",
    "racism",
    "racial",
    "racist")

dfm_keywords_ha <-
     dfm_select(all_text_c_ha_dtm,
                pattern = keywords,
                selection = "keep")

# similarities for specific documents for keywords

# compute similarities between features using relative frequency
simi_all_ha <-
  all_text_c_ha_dtm %>%
  dfm_select(min_nchar = 4) %>%
  dfm_trim(min_termfreq = 100,
           min_docfreq = 5) %>%
  dfm_weight(scheme = "prop") %>%
  textstat_simil(y = dfm_keywords_ha,
                 method = "jaccard",
                 margin = "features")

# tidy
list_simil_ha <- as.list(simi_all_ha, n = 20)

# list_simil

list_simil_ha_lst <-
  map(list_simil_ha, ~tibble(corval = .x, feat = names(.x)))

list_simil_ha_tbl <-
  bind_rows(list_simil_ha_lst, .id = "kw") %>%
  group_by(kw) %>%
  mutate(id = row_number())

# plot

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# this is the figure included in the manuscript
library(ggrepel)
ha_p_3 <- ggplot(list_simil_ha_tbl) +
  aes( kw, id) +
  geom_text(aes(label = feat,
                size = corval,
                colour = corval)) +
  scale_colour_gradient(low = "blue",
                        high = "red",
                        name = "Similarity") +
  scale_y_reverse() +
  scale_size_continuous(range = c(5, 9)) +
  scale_x_discrete(position = "top") +
  theme_classic() +
  theme(axis.text.x.top  = element_text(size = 20)) +
  labs(x = "",
       y = "rank") +
  guides(size = FALSE)

# ggsave(here::here("analysis/figures/003-keyword-similar-words.jpg"),
#        h = 4,
#        w = 10,
#        scale = 4,
#        units = "cm",
#        dpi = "retina")



pngfile_3 <- here::here("analysis/figures/003-ha-keyword-similar-words.png")
jpgfile_3 <- here::here("analysis/figures/003-ha-keyword-similar-words.jpg")

library(ragg)

# write PNG file with desired size and resolution
agg_png(pngfile_3,
        width = 8,
        height = 5,
        units = "cm",
        res = 1000,
        scaling = 0.3)
ha_p_3

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in_3 <- image_read(pngfile_3)
png_3_jpg_3 <- image_convert(img_in_3, "jpg")
image_write(png_3_jpg_3, jpgfile_3, density = 1000, quality = 100)



