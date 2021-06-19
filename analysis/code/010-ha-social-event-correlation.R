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

# read in all txt files of HA abstracts
ha_abstracts <- read_csv("analysis/data/ha_titles_abstracts_years_df.csv")

# count all words for each year
all_text_c_ha <- corpus(ha_abstracts$abstract)
docnames(all_text_c_ha) <-  ha_abstracts$year

# make a dfm
all_text_c_ha_dtm <-
  all_text_c_ha %>%
  tokens(
    verbose = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    split_hyphens = TRUE,
    remove_punct = TRUE) %>%
  dfm()

# Explore key words over time
keywords <-
  c("race",
    "racism",
    "racial",
    "racist")

dfm_keywords <-
  dfm_select(all_text_c_ha_dtm,
             pattern = keywords,
             selection = "keep")

ha_words_per_year <-
  tibble(year = as.numeric(names(rowSums(dfm_keywords))),
         ha_wordcount = rowSums(dfm_keywords)) %>%
  mutate(year = parse_number(str_sub(year, 1, 4))) %>%
  group_by(year) %>%
  summarise(ha_wordcount = sum(ha_wordcount, na.rm = TRUE))

n_words <- sum(ha_words_per_year$ha_wordcount, na.rm = TRUE )
n_abstracts <- length(rowSums(dfm_keywords))

# visualise
gg1 <-
  ggplot(ha_words_per_year) +
  aes(x = year,
      y = ha_wordcount) +
  geom_col() +
  theme_bw(base_size = 8) +
  labs(x = "Year",
       y = paste0("HA abstracts (n = ", n_words, ")")
       # title = "Histogram of events from Wikipedia's 'Timeline of African-American history'",
       # subtitle = "data from https://en.wikipedia.org/wiki/Timeline_of_African-American_history"
  )


# join HA and history data
ha_and_history_tbl <-
  event_all_tbl_tally %>%
  right_join(ha_words_per_year) %>%
  arrange(year) %>%
  replace_na(list(n = 0, ha_wordcount = 0)) %>%
  mutate(
    `Same year` = ha_wordcount,
    `1 year lag` = lag(ha_wordcount),
    `2 year lag` = lag(ha_wordcount, 2),
    `3 year lag` = lag(ha_wordcount, 3),
    `4 year lag` = lag(ha_wordcount, 4),
    `5 year lag` = lag(ha_wordcount, 5),
    `6 year lag` = lag(ha_wordcount, 6)
  ) %>%
  select(-ha_wordcount,
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
# lag years when race appeared in an HA abstract
n_events_subset <-
  event_all_tbl_tally %>%
  right_join(ha_words_per_year) %>%
  arrange(year) %>%
  filter(ha_wordcount != 0) %>%
  pull(n) %>%
  sum(na.rm = TRUE)

# how many events per plot?
ha_and_history_tbl_per_lag_plot <-
  ha_and_history_tbl %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  summarise(n_events = sum(n, na.rm = TRUE),
            n_words = sum(value, na.rm = TRUE))

ha_and_history_tbl <-
  ha_and_history_tbl %>%
  left_join(ha_and_history_tbl_per_lag_plot) %>%
  mutate(facet_label = paste0(name, "\n(", n_events, " events, ", n_words, " words)"))


# scatter plot
library(ggpubr)
library(ggrepel)

sp <-
  ggplot(ha_and_history_tbl) +
  aes(n,
      value) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = year),
                  size = 4) +
  geom_smooth(se = FALSE,
              method = "lm") +
  stat_cor(label.y = 10,
           label.x = 4.3,
           size = 3) +
  stat_regline_equation(label.y = 11,
                        label.x = 4.3,
                        size = 3) +
  theme_bw(base_size = 12) +
  labs(x = paste0("African-American historical event annual frequency"),
       y = paste0("Mentions of 'race', etc. (total of ", n_words, ")\nin HA abstracts (n = ", n_abstracts, ")")) +
  facet_wrap( ~ facet_label) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# looks like a significant correlation at the 3 year lag, get the details
ha_and_history_tbl_3yr <-
ha_and_history_tbl %>%
  filter(name == "3 year lag")

ha_and_history_tbl_3yr_aov <- aov(value ~ n, data = ha_and_history_tbl_3yr)
ha_and_history_tbl_3yr_lm <- lm(value ~ n, data = ha_and_history_tbl_3yr)

apa::anova_apa(ha_and_history_tbl_3yr_aov)
ha_and_history_tbl_3yr_lm_summary <- summary(ha_and_history_tbl_3yr_lm)

adjusted_r_squared <- ha_and_history_tbl_3yr_lm_summary$adj.r.squared

library(patchwork)
p_5 <-  sp

pngfile_5 <- here::here("analysis/figures/005-ha-keyword-and-event-relationships.png")
jpgfile_5 <- here::here("analysis/figures/005-ha-keyword-and-event-relationships.jpg")

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
ha_and_protest_history_tbl <-
  protest_event_all_tbl_tally %>%
  right_join(ha_words_per_year) %>%
  arrange(year) %>%
  replace_na(list(n = 0, ha_wordcount = 0)) %>%
  mutate(
    `Same year` = ha_wordcount,
    `1 year lag` = lag(ha_wordcount),
    `2 year lag` = lag(ha_wordcount, 2),
    `3 year lag` = lag(ha_wordcount, 3),
    `4 year lag` = lag(ha_wordcount, 4),
    `5 year lag` = lag(ha_wordcount, 5),
    `6 year lag` = lag(ha_wordcount, 6)
  ) %>%
  select(-ha_wordcount,
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
  right_join(ha_words_per_year) %>%
  arrange(year) %>%
  filter(ha_wordcount != 0) %>%
  pull(n) %>%
  sum(na.rm = TRUE)

# how many events per plot?
ha_and_protest_history_tbl_per_lag_plot <-
  ha_and_protest_history_tbl %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  summarise(n_events = sum(n, na.rm = TRUE),
            n_words = sum(value, na.rm = TRUE))

ha_and_protest_history_tbl <-
  ha_and_protest_history_tbl %>%
  left_join(ha_and_protest_history_tbl_per_lag_plot) %>%
  mutate(facet_label = paste0(name, "\n(", n_events, " events, ", n_words, " words)"))

# protest events scatter plot
library(ggpubr)
library(ggrepel)

protest_sp <-
  ggplot(ha_and_protest_history_tbl) +
  aes(n,
      value) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = year),
                  size = 4) +
  geom_smooth(se = FALSE,
              method = "lm") +
  stat_cor(label.y = 11,
           label.x = 1,
           size = 3) +
  stat_regline_equation(label.y = 12,
                        label.x = 1,
                        size = 3) +
  theme_bw(base_size = 12) +
  labs(x = paste0("African-American historical event annual frequency (protests only)"),
       y = paste0("Mentions of 'race', etc. (total of ", n_words, ")\nin HA abstracts (n = ", n_abstracts, ")")) +
  facet_wrap( ~ facet_label, scales = "free_x") +
  scale_y_continuous(breaks = scales::pretty_breaks())

# put both plots together
library(patchwork)
p_protest <- gg_protest + protest_sp + plot_layout(ncol = 1,
                                                   heights = c(0.3, 1))

# inspect some diagnostics of the significant plots
# guide to interpretation:
# https://data.library.virginia.edu/diagnostic-plots/
# n is protest event
# value is word count
saa_and_protest_history_tbl_5yr <-
  saa_and_protest_history_tbl %>%
  filter(name == "5 year lag")

five_year <- lm(n ~ value, data = saa_and_protest_history_tbl_5yr)
summary(five_year)
library(ggfortify)
autoplot(five_year, label.size = 5)

saa_and_protest_history_tbl_6yr <-
  saa_and_protest_history_tbl %>%
  filter(name == "6 year lag")

six_year <- lm(n ~ value, data = saa_and_protest_history_tbl_6yr)
summary(six_year)
library(ggfortify)
autoplot(six_year, label.size = 5)




