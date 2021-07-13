
social_event_correlation_plots_fn <- function(){

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

# save a copy of the Wikipedia table locally for future reference
write_csv(event_all_tbl,
          here::here("analysis/data/Timeline_of_African-American_history_social_events.csv"))

#------------------------------------------------------------------
# or read in our local copy of the Wikipedia table that we made above
event_all_tbl <-
  read_csv(here::here("analysis/data/Timeline_of_African-American_history_social_events.csv"))

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
if(!exists("all_text_clean")){
  all_text_clean <- readRDS(here::here("analysis","data", "all_text_clean.rds"))
}

# count all words for each year
if(!exists("all_text_c")){
  all_text_c <- corpus(all_text_clean)
}

# make a dfm
if(!exists("all_text_c_dtm")){
    all_text_c_dtm <- readRDS(here::here("analysis/data/all_text_c_dtm.rds"))
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
# which are events that also happened in a year or
# lag years when race appeared in an SAA abstract
n_events_subset <-
event_all_tbl_tally %>%
  right_join(saa_words_per_year) %>%
  arrange(year) %>%
  filter(saa_wordcount != 0) %>%
  pull(n) %>%
  sum(na.rm = TRUE)

# how many events per plot?
saa_and_history_tbl_per_lag_plot <-
saa_and_history_tbl %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  summarise(n_events = sum(n, na.rm = TRUE),
            n_words = sum(value, na.rm = TRUE))

saa_and_history_tbl <-
  saa_and_history_tbl %>%
  left_join(saa_and_history_tbl_per_lag_plot) %>%
  mutate(facet_label = paste0(name, "\n(", n_events, " events, ", n_words, " words)"))


# scatter plot
library(ggpubr)
library(ggrepel)

sp <-
ggplot(saa_and_history_tbl) +
  aes(n,
      value) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = year),
            size = 4) +
  geom_smooth(se = FALSE,
              method = "lm") +
  stat_cor(label.y = 70,
           label.x = 8,
           size = 3) +
  stat_regline_equation(label.y = 65,
                        label.x = 8,
                        size = 3) +
  theme_bw(base_size = 12) +
  labs(x = paste0("African-American historical event annual frequency"),
       y = paste0("Mentions of 'race', etc. (total of ", n_words, ")\nin SAA abstracts (n = ", n_abstracts, ")")) +
  facet_wrap( ~ facet_label) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# put both plots together
library(patchwork)
p_5 <- gg + sp + plot_layout(ncol = 1,
                      heights = c(0.3, 1))

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

print(p_5)

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in_5 <- image_read(pngfile_5)
png_5_jpg_5 <- image_convert(img_in_5, "jpg")
image_write(png_5_jpg_5, jpgfile_5, density = 1000, quality = 100)


# --------------------------------------------------------------
# what about looking only at protest events that might stimulate
# expressions of solidarity

protest_event_all_tbl <-
  event_all_tbl %>%
  filter(str_detect(event_all, "protest"))

# events per year
protest_event_all_tbl_tally <-
  protest_event_all_tbl %>%
  group_by(year) %>%
  tally(sort = TRUE)

protest_event_all_tbl_tally <-
  protest_event_all_tbl_tally %>%
  full_join(
    tibble(year = seq(min(protest_event_all_tbl_tally$year),
                      max(protest_event_all_tbl_tally$year),
                      1))
  )

n_protest_events = sum(protest_event_all_tbl_tally$n, na.rm = TRUE)

# visualise
min_year <- min(protest_event_all_tbl$year)
max_year <- max(protest_event_all_tbl$year)

gg_protest <-
  ggplot(protest_event_all_tbl) +
  aes(year) +
  geom_histogram(binwidth = 1) +
  theme_bw(base_size = 8) +
  scale_x_continuous(
    breaks = seq(1550, max_year,
                 by = 50)) +
  labs(x = "Year",
       y = paste0("African-American\nhistorical protest event annual\nfrequency (n = ", n_protest_events, ")")
       # title = "Histogram of events from Wikipedia's 'Timeline of African-American history'",
       # subtitle = "data from https://en.wikipedia.org/wiki/Timeline_of_African-American_history"
  )

# join SAA and protest history data
saa_and_protest_history_tbl <-
  protest_event_all_tbl_tally %>%
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
# which are events that also happened in a year or
# lag years when race appeared in an SAA abstract
n_protest_events_subset <-
  protest_event_all_tbl_tally %>%
  right_join(saa_words_per_year) %>%
  arrange(year) %>%
  filter(saa_wordcount != 0) %>%
  pull(n) %>%
  sum(na.rm = TRUE)

# how many events per plot?
saa_and_protest_history_tbl_per_lag_plot <-
  saa_and_protest_history_tbl %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  summarise(n_events = sum(n, na.rm = TRUE),
            n_words = sum(value, na.rm = TRUE))

saa_and_protest_history_tbl <-
  saa_and_protest_history_tbl %>%
  left_join(saa_and_protest_history_tbl_per_lag_plot) %>%
  mutate(facet_label = paste0(name, "\n(", n_events, " events, ", n_words, " words)"))

# protest events scatter plot
library(ggpubr)
library(ggrepel)

protest_sp <-
  ggplot(saa_and_protest_history_tbl) +
  aes(n,
      value) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = year),
                  size = 4) +
  geom_smooth(se = FALSE,
              method = "lm") +
  stat_cor(label.y = 16,
           label.x = 0.5,
           size = 3) +
  stat_regline_equation(label.y = 15,
                        label.x = 0.5,
                        size = 3) +
  theme_bw(base_size = 12) +
  labs(x = paste0("African-American historical event annual frequency (protests only)"),
       y = paste0("Mentions of 'race', etc. (total of ", n_words, ")\nin SAA abstracts (n = ", n_abstracts, ")")) +
  facet_wrap( ~ facet_label, scales = "free_x")

# put both plots together
library(patchwork)
p_protest <- gg_protest + protest_sp + plot_layout(ncol = 1,
                             heights = c(0.3, 1))

# inspect some diagnostics of the significant plots
# guide to interpretation:
# https://data.library.virginia.edu/diagnostic-plots/
# n is protest event
# value is word count

# 5 year lag
saa_and_protest_history_tbl_5yr <-
  saa_and_protest_history_tbl %>%
  filter(name == "5 year lag")

row.names(saa_and_protest_history_tbl_5yr) <- saa_and_protest_history_tbl_5yr$year
five_year <- lm(value ~ n, data = saa_and_protest_history_tbl_5yr)

summary(five_year)

library(performance)
# performance pkg requires pkg 'see' and 'qqlotr'

p_6 <- check_model(five_year, dot_size =2)


#save the diagnostic plots
pngfile_6 <- here::here("analysis/figures/005-5-year-lag-diagnostic.png")
jpgfile_6 <- here::here("analysis/figures/005-5-year-lag-diagnostic.jpg")

library(ragg)

# write PNG file with desired size and resolution
agg_png(pngfile_6,
        width = 10,
        height = 12,
        units = "cm",
        res = 1000,
        scaling = 0.5)

print(p_6)

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in_6 <- image_read(pngfile_6)
png_6_jpg_6 <- image_convert(img_in_6, "jpg")
image_write(png_6_jpg_6, jpgfile_6, density = 1000, quality = 100)


# 6 year lag
saa_and_protest_history_tbl_6yr <-
  saa_and_protest_history_tbl %>%
  filter(name == "6 year lag")

six_year <- lm(value ~ n, data = saa_and_protest_history_tbl_6yr)
summary(six_year)

p_7 <- check_model(six_year, dot_size =2)


#save the diagnostic plots
pngfile_7 <- here::here("analysis/figures/005-6-year-lag-diagnostic.png")
jpgfile_7 <- here::here("analysis/figures/005-6-year-lag-diagnostic.jpg")


# write PNG file with desired size and resolution
agg_png(pngfile_7,
        width = 10,
        height = 12,
        units = "cm",
        res = 1000,
        scaling = 0.5)

print(p_7)

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in_7 <- image_read(pngfile_7)
png_7_jpg_7 <- image_convert(img_in_7, "jpg")
image_write(png_7_jpg_7, jpgfile_7, density = 1000, quality = 100)


# looks like a significant correlation at the 5 and 6 year lag, get the details

# five year lag
saa_protest_five_year_aov <-  aov(value ~ n, data = saa_and_protest_history_tbl_5yr)
saa_protest_five_year_lm <- lm(value ~ n, data = saa_and_protest_history_tbl_5yr)

apa::anova_apa(saa_protest_five_year_aov)
ha_and_history_tbl_5yr_lm_summary <- summary(saa_protest_five_year_lm)

adjusted_r_squared <- ha_and_history_tbl_5yr_lm_summary$adj.r.squared

# six year lag
saa_protest_six_year_aov <-  aov(value ~ n, data = saa_and_protest_history_tbl_6yr)
saa_protest_six_year_lm <- lm(value ~ n, data = saa_and_protest_history_tbl_6yr)

apa::anova_apa(saa_protest_six_year_aov)
ha_and_history_tbl_6yr_lm_summary <- summary(saa_protest_six_year_lm)

adjusted_r_squared <- ha_and_history_tbl_6yr_lm_summary$adj.r.squared

}




