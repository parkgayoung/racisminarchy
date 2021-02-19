library(rvest)
library(tidyverse)

# get data from Wikipedia of social events about African-Americans
url <- "https://en.wikipedia.org/wiki/Timeline_of_African-American_history"

event_desc_year <- ".tright+ ul li , p+ ul li , p b"

event_all <-
  url %>%
  read_html() %>%
  html_nodes(event_desc_year) %>%
  html_text()

# tidy it into a data frame
event_all_tbl <-
  tibble(event_all = event_all,
         year = 0) %>%
  mutate(year = ifelse(nchar(event_all) == 4, event_all, NA))  %>%
  mutate(year = as.numeric(year)) %>%
  fill(year) %>%
  filter(nchar(event_all) != 4)

# events per year
event_all_tbl_tally <-
event_all_tbl %>%
  group_by(year) %>%
  tally(sort = TRUE)

event_all_tbl_tally <-
event_all_tbl_tally %>%
  full_join(
  tibble(year = seq(min(event_all_tbl_tally$year),
                    max(event_all_tbl_tally$year),
                    1))
  )

n_events = sum(event_all_tbl_tally$n, na.rm = TRUE)

# visualise
min_year <- min(event_all_tbl$year)
max_year <- max(event_all_tbl$year)

gg <-
ggplot(event_all_tbl) +
  aes(year) +
  geom_histogram(binwidth = 1) +
  theme_bw(base_size = 8) +
  scale_x_continuous(
                     breaks = seq(1550, max_year,
                     by = 50)) +
  labs(x = "Year",
       y = paste0("African-American\nhistorical event annual\nfrequency (n = ", n_events, ")")
      # title = "Histogram of events from Wikipedia's 'Timeline of African-American history'",
      # subtitle = "data from https://en.wikipedia.org/wiki/Timeline_of_African-American_history"
       )

# gg

# plotly::ggplotly(gg)

# compare with words and years in SAA abstracts

library(tidyverse)
library(quanteda)

# read in all txt files of SAA abstracts from 1962 to 2020. Data is from SAA website and scanned images of text is converted to PDF using OCR
# the code for OCR is here: https://github.com/benmarwick/saa-meeting-abstracts/blob/master/code/001-PDF-page-images-to-txt.R
# this is a character vector, one abstract per element
if(!exists("all_text")){
  all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))
}

# count all words for each year
if(!exists("all_text_c")){
  all_text_c <- corpus(all_text)
}

# make a dfm
if(!exists("all_text_c_dtm")){
  all_text_c_dtm <-
    dfm(all_text_c,
        remove = stopwords("english"),
        # stem = TRUE,
        verbose = TRUE,
        remove_punct = TRUE)

}

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

saa_words_per_year <-
  tibble(year = as.numeric(names(rowSums(dfm_keywords))),
         saa_wordcount = rowSums(dfm_keywords))

n_words <- sum(saa_words_per_year$saa_wordcount, na.rm = TRUE )
n_abstracts <- length(rowSums(dfm_keywords))

# visualise
gg1 <-
  ggplot(saa_words_per_year) +
  aes(x = year,
      y = saa_wordcount) +
  geom_col() +
  theme_bw(base_size = 8) +
  labs(x = "Year",
       y = paste0("SAA annual\nfrequency (n = ", n_words, ")")
       # title = "Histogram of events from Wikipedia's 'Timeline of African-American history'",
       # subtitle = "data from https://en.wikipedia.org/wiki/Timeline_of_African-American_history"
  )



# join SAA and history data
saa_and_history_tbl <-
  event_all_tbl_tally %>%
  right_join(saa_words_per_year) %>%
  arrange(year) %>%
  replace_na(list(n = 0, saa_wordcount = 0)) %>%
  mutate(
    `Same year` = saa_wordcount,
    `1 year lag` = lag(saa_wordcount),
    `2 year lag` = lag(saa_wordcount, 2),
    `3 year lag` = lag(saa_wordcount, 3),
    `4 year lag` = lag(saa_wordcount, 4),
    `5 year lag` = lag(saa_wordcount, 5),
    `6 year lag` = lag(saa_wordcount, 6)
    ) %>%
  select(-saa_wordcount,
         -`Same year`)  %>%
  pivot_longer(-c(year, n)) %>%
  mutate(name = factor(name,
                       levels = c(
                                  "1 year lag",
                                  "2 year lag",
                                  "3 year lag",
                                  "4 year lag",
                                  "5 year lag",
                                  "6 year lag")))

# African-American events on the scatterplot
n_events_subset <-
saa_and_history_tbl %>%
  distinct(year, n) %>%
  pull(n) %>%
  sum()

# scatter plot
library(ggpubr)
sp <-
ggplot(saa_and_history_tbl) +
  aes(n,
      value) +
  geom_text(aes(label = year),
            size = 2) +
  geom_smooth(se = FALSE,
              method = "lm") +
  stat_cor(label.y = 75,
           label.x = 8,
           size = 3) +
  stat_regline_equation(label.y = 65,
                        label.x = 8,
                        size = 3) +
  theme_bw(base_size = 12) +
  labs(x = paste0("African-American historical event annual frequency (n = ", n_events_subset, ")"),
       y = paste0("Mentions of 'race', etc. (n = ", n_words, ")\nin SAA abstracts (n = ", n_abstracts, ")")) +
  facet_wrap( ~ name)

# put both plots together
library(patchwork)
p_5 <- gg + sp + plot_layout(ncol = 1,
                      heights = c(0.3, 1))

ggsave(here::here("analysis/figures/005-keyword-and-event-relationships.jpg"),
        h = 8,
        w = 10,
        scale = 2.7,
        units = "cm")


pngfile_5 <- here::here("analysis/figures/005-keyword-and-event-relationships.png")
jpgfile_5 <- here::here("analysis/figures/005-keyword-and-event-relationships.jpg")

library(ragg)

# write PNG file with desired size and resolution
agg_png(pngfile_5,
        width = 13,
        height = 10,
        units = "cm",
        res = 1000,
        scaling = 0.5)
p_5

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in_5 <- image_read(pngfile_5)
png_5_jpg_5 <- image_convert(img_in_5, "jpg")
image_write(png_5_jpg_5, jpgfile_5, density = 1000, quality = 100)

