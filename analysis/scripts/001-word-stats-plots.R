
word_stats_plots_fn <- function(){

library(tidyverse)
library(quanteda)

# load data that we previously prepared in 000-
all_text_c_dtm <- readRDS(here::here("analysis/data/all_text_c_dtm.rds"))
all_txts_c_summary <- readRDS(here::here("analysis/data/all_txts_c_summary.rds"))

# Explore key words over time
keywords <-
  c("race",
    "racism",
    "racial",
    "racist")

dfm_keywords <-
  dfm_select(all_text_c_dtm,
             pattern = keywords,
             selection = "keep")

dfm_keywords_tbl <-
  convert(dfm_keywords, to = "data.frame") %>%
  pivot_longer(-doc_id,
    names_to = "keyword",
               values_to = "n") %>%
  mutate(year = parse_number(doc_id)) %>%
  group_by(keyword) %>%
  mutate(sum = sum(n),
         mean = mean(n),
         cumsum = cumsum(n)) %>%
  mutate(keyword_n = str_c(keyword, " (n = ", sum, ")"))

# group keywords
dfm_keywords_tbl_groups <-
  convert(dfm_keywords, to = "data.frame") %>%
  mutate(`race/racial/racism/racist` = racial + race + racism + racist) %>%
  select(doc_id,
         `race/racial/racism/racist`) %>%
  pivot_longer(-doc_id,
               names_to = "keyword",
               values_to = "n") %>%
  mutate(year = parse_number(doc_id)) %>%
  group_by(keyword) %>%
  mutate(sum_keyword = sum(n),
         mean_keyword = mean(n),
         cumsum_keyword = cumsum(n)) %>%
  mutate(keyword_n = str_c(keyword, " (n = ", sum_keyword, ")"))

#-----------------------------------------------------------------------

library(readxl)
saa_abstract <-
  read_excel(here::here("analysis","data", "saa-abstracts-tally.xlsx"))

n_abstracts <- sum(saa_abstract$number_of_abstracts, na.rm=T)

# compute proportion of all word groups per year and sum up the keywords

dfm_keywords_tbl_prop <-
  dfm_keywords_tbl_groups %>%
  left_join(all_txts_c_summary) %>%
  mutate(prop = n / Tokens ) %>%
  group_by(keyword) %>%
  filter(!is.na(prop)) %>%
  mutate(max_per_class = ifelse(prop == max(prop),
                                str_c("maximum of ", n, " in ", year), NA)) %>%
  arrange(sum_keyword, max_per_class) %>%
  fill(max_per_class) %>%
  mutate(keyword_sets = paste0(keyword, ", n = ", sum_keyword, ", ", max_per_class))

# plot of keywords as a proportion of all words per year
# this is the figure included in the manuscript
keyword_proportion_per_year <-
  ggplot(data = dfm_keywords_tbl_prop,
         aes(x = year ,
             y = prop)) +
  geom_col() +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)),
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(name = "Proportion of all words per year",
                     labels = scales::comma) +
  theme_minimal(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5),
        strip.text = element_text(size = 30)) +
  ggtitle(paste0("Race words per year in SAA Meeting abstracts (", dfm_keywords_tbl_prop$keyword_sets[1], ")"))


#-----------------------------------------------------------------------

all_txts_c_summary_join_abstract <-
  all_txts_c_summary %>%
  left_join(saa_abstract) %>%
  select(Text, Tokens, number_of_abstracts, year) %>%
  mutate(`words/abstract` = Tokens/number_of_abstracts)

# two figures to be combined to the first figure in the manuscript
# all word per year
all_words_per_year <-
  ggplot(data = all_txts_c_summary_join_abstract,
         aes(x = year,
             y = Tokens)) +
  geom_col() +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)),
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(labels = scales::comma(c(seq(0, 800000, 200000))),
                     breaks = seq(0, 800000, 200000),
                     name = "all words") +
  theme_minimal(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  ggtitle("Total words per year in SAA Meeting abstracts")

# all word per abstract
all_words_per_abstracts_per_year <-
  ggplot(data = all_txts_c_summary_join_abstract,
         aes(x = year,
             y = `words/abstract`)) +
  geom_col() +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)),
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(labels = c(seq(0, 180, 50)),
                     breaks = seq(0, 180, 50),
                     name = "words/abstract") +
  theme_minimal(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  ggtitle("Average words per abstract in SAA Meeting abstracts")

# combine three plots
library(cowplot)

p <-
plot_grid(all_words_per_year,
          all_words_per_abstracts_per_year,
          keyword_proportion_per_year,
          rel_heights = c(0.5, 0.5, 0.5),
          labels = c('A', 'B', 'C'),
          label_size = 30,
          align = "v",
          axis = "lr",
          ncol = 1)

pngfile <- here::here("analysis/figures/001-keyword-time-series.png")
jpgfile <- here::here("analysis/figures/001-keyword-time-series.jpg")

library(ragg)

# write PNG file with desired size and resolution
agg_png(pngfile,
        width = 12,
        height = 9,
        units = "cm",
        res = 1000,
        scaling = 0.2)

print(p)

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in <- image_read(pngfile)
png_2_jpg <- image_convert(img_in, "jpg")
image_write(png_2_jpg, jpgfile, density = 1000, quality = 100)

# check the JPG, should be 300 dpi
# may need to adjust base_size, label_size, element_text(size, and other
# text size values to make it look the right size in the JPG

# we want to use this object in our HA abstracts analysis
return(dfm_keywords_tbl_prop)

}






