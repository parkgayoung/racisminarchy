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

# visualise
gg <-
ggplot(event_all_tbl) +
  aes(year) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  labs(x = "Year",
       y = "Number of events",
       title = "Histogram of events from Wikipedia's 'Timeline of African-American history'",
       subtitle = "data from https://en.wikipedia.org/wiki/Timeline_of_African-American_history")

gg

plotly::ggplotly(gg)

# compare with words and years in SAA abstracts

library(tidyverse)
library(quanteda)

# read in all txt files of SAA abstracts from 1962 to 2020. Data is from SAA website and scanned images of text is converted to PDF using OCR
# the code for OCR is here: https://github.com/benmarwick/saa-meeting-abstracts/blob/master/code/001-PDF-page-images-to-txt.R
# this is a character vector, one abstract per element
all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))

# count all words for each year
all_text_c <- corpus(all_text)

# make a dfm
all_text_c_dtm <-
  dfm(all_text_c,
      remove = stopwords("english"),
      # stem = TRUE,
      verbose = TRUE,
      remove_punct = TRUE)

saveRDS(all_text_c_dtm,
        here::here("analysis","data", "all_text_c_dtm.rds"))

# Explore key words over time
keywords <-
  c("race",
    "racism",
    "discrimination",
    "inequality",
    "inequalities")

dfm_keywords <-
  dfm_select(all_text_c_dtm,
             pattern = keywords,
             selection = "keep")

saa_words_per_year <-
  tibble(year = as.numeric(names(rowSums(dfm_keywords))),
         saa_wordcount = rowSums(dfm_keywords))

# join SAA and history data
saa_and_history_tbl <-
  event_all_tbl_tally %>%
  left_join(saa_words_per_year) %>%
  drop_na %>%
  mutate(`Social event annual frequency` = n,
         `SAA abstract word frequency` = saa_wordcount)

# scatter plot
library(ggpubr)
ggplot(saa_and_history_tbl) +
  aes(`Social event annual frequency`,
      `SAA abstract word frequency`) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE,
              method = "lm") +
  stat_cor(label.y = 75,
           label.x = 12,
           size = 5) +
  stat_regline_equation(label.y = 65,
                        label.x = 12,
                        size = 5) +
  theme_minimal(base_size = 14)


