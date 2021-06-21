

ha_saa_compare_plot_fn <- function(word_stats_plots){

library(tidyverse)
library(quanteda)

# we acquired the data by scraping the website, more details in 006-ha-abstracts.R

ha_abstracts <- read_csv("analysis/data/ha_titles_abstracts_years_df.csv")

# how many abstracts?
nrow(ha_abstracts)

# spanning what years?
range(ha_abstracts$year)

# our steps

# count all words for each year
all_text_c_ha <- corpus(ha_abstracts$abstract)
docnames(all_text_c_ha) <-  ha_abstracts$year

# make a dfm (skip this, and run line 26)
all_text_c_ha_dtm <-
  all_text_c_ha %>%
  tokens(
    verbose = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    split_hyphens = TRUE,
    remove_punct = TRUE) %>%
  dfm()

ha_word_count_per_abstract <- ntoken(all_text_c_ha)

# all text into df
all_text_c_ha_dtm_df <-
convert(all_text_c_ha_dtm,
        to = "data.frame") %>%
  pivot_longer(-doc_id,
               names_to = "keyword",
               values_to = "n") %>%
  mutate(year = parse_number(str_sub(doc_id, 1, 4)))

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

n_keywords_ha <- sum(ntoken(dfm_keywords_ha))

dfm_keywords_ha_tbl <-
  convert(dfm_keywords_ha, to = "data.frame") %>%
  pivot_longer(-doc_id,
               names_to = "keyword",
               values_to = "n") %>%
  mutate(year = parse_number(str_sub(doc_id, 1, 4))) %>%
  group_by(keyword) %>%
  mutate(sum = sum(n)) %>%
  mutate(keyword_n = str_c(keyword, " (n = ", sum, ")"),
         total_words_in_abstract = ha_word_count_per_abstract) %>%
  group_by(year) %>%
  summarise(annual_n_keywords = sum(n),
            annual_abstracts = n(),
            annual_n_words_in_all_abstracts = sum(total_words_in_abstract),
            annual_n_words_per_abstract = annual_n_words_in_all_abstracts / annual_abstracts,
            prop_keywords = annual_n_keywords / annual_n_words_in_all_abstracts)

# How many after 2010?
perc_of_keywords_after_2010 <-
dfm_keywords_ha_tbl %>%
  filter(year >= 2010) %>%
  pull(annual_n_keywords) %>%
  sum / n_keywords_ha * 100

# plot of keywords as a proportion of all words per year
# this is the figure included in the manuscript
ha_keyword_proportion_per_year <-
  ggplot(data = dfm_keywords_ha_tbl,
         aes(x = year ,
             y = prop_keywords)) +
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
  ggtitle(paste0("Race words per year in HA article abstracts (race/racial/racism/racist, n = ",
                 n_keywords_ha, ")"))

ha_all_words_per_year <-
  ggplot(data = dfm_keywords_ha_tbl,
         aes(x = year,
             y = annual_n_words_in_all_abstracts)) +
  geom_col() +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)),
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(
                     name = "all words") +
  theme_minimal(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  ggtitle("Total words per year in HA article abstracts")

# all word per abstract
ha_all_words_per_abstracts_per_year <-
  ggplot(data = dfm_keywords_ha_tbl,
         aes(x = year,
             y = annual_n_words_per_abstract)) +
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
  ggtitle("Average words per abstract in HA article abstracts")

# combine three plots
library(cowplot)

ha_p <-
  plot_grid(ha_all_words_per_year,
            ha_all_words_per_abstracts_per_year,
            ha_keyword_proportion_per_year,
            rel_heights = c(0.5, 0.5, 0.5),
            labels = c('A', 'B', 'C'),
            label_size = 30,
            align = "v",
            axis = "lr",
            ncol = 1)

# better comparison is a boxplot of SAA and HA counts

# get this from our earlier analysis of the SAA keywords
dfm_keywords_tbl_prop <- word_stats_plots

both_associations <-
tibble(
  source = c(rep("Historical Archaeology",  nrow(dfm_keywords_ha_tbl)),
             rep("Society of American Archaeology", nrow(dfm_keywords_tbl_prop))),

  prop =  c(dfm_keywords_ha_tbl$prop_keywords,
         dfm_keywords_tbl_prop$prop)
) %>%
  filter(prop > 0)

library(ggbeeswarm)
both_associations_plot <-
ggplot(both_associations) +
  aes(source,
      prop) +
  geom_boxplot() +
  geom_beeswarm(alpha = 0.3,
                size = 5,
                cex = 4) +
  scale_y_log10(name = "Proportion of all words per year",
                     labels = scales::comma) +
  theme_minimal(base_size = 16) +
  labs(x = "")

pngfile <- here::here("analysis/figures/006-ha-saa-boxplot.png")
jpgfile <- here::here("analysis/figures/006-ha-saa-boxplot.jpg")

library(ragg)

# write PNG file with desired size and resolution
agg_png(pngfile,
        width = 4,
        height = 3,
        units = "cm",
        res = 1000,
        scaling = 0.2)

print(both_associations_plot)

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in <- image_read(pngfile)
png_2_jpg <- image_convert(img_in, "jpg")
image_write(png_2_jpg, jpgfile, density = 1000, quality = 100)

}






