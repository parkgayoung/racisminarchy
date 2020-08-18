library(tidyverse)
library(quanteda)

# read in all txt files of SAA abstracts from 1962 to 2020. Data is from SAA website and scanned images of text is converted to PDF using OCR
# the code for OCR is here: https://github.com/benmarwick/saa-meeting-abstracts/blob/master/code/001-PDF-page-images-to-txt.R
# this is a character vector, one abstract per element
# start the code from line 23 (skip the code before that line to save time)

all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))

# count all words for each year
all_text_c <- corpus(all_text)

# make a dfm
all_text_c_dtm <-
  dfm(all_text_c,
      remove = stopwords("english"),
      stem = TRUE,
      verbose = TRUE,
      remove_punct = TRUE)

# read from data file
all_text_c_dtm <-
  readRDS(here::here("analysis","data", "all_text_c_dtm.rds"))

# Explore key words over time
keywords <-
  c("race",
    "racism",
    "racial",
    "racist",
    "inequality",
    "inequalities",
    "discrimination",
    "discriminatory")

dfm_keywords <-
  dfm_select(all_text_c_dtm,
             pattern = keywords,
             selection = "keep")
# saveRDS(dfm_keywords, here::here("analysis","data","dfm_keywords.rds"))

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

# make hline for cumsum
cumsum_single_word <-
  dfm_keywords_tbl %>%
  filter(cumsum > sum/2 ) %>%
  group_by(keyword_n) %>%
  filter(cumsum == min(cumsum))

# plot of frequency of keywords per year
ggplot(data = dfm_keywords_tbl,
       aes(x = year ,
           y = n)) +
  geom_col() +
  geom_vline(data = cumsum_single_word,
             aes(xintercept = year),
             color = "red") +
  facet_wrap( ~ keyword_n,
              ncol = 1,
              scales = "free_y") +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)),
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(name = "Frequency of keyword per year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

# group keywords to three: race, inequality, and discrimination
dfm_keywords_tbl_groups <-
  convert(dfm_keywords, to = "data.frame") %>%
  mutate(`race/racial/racism/racist` = racial + race + racism + racist,
         `inequality/inequalities` = inequality + inequalities,
         `discrimination/discriminatory` = discrimination + discriminatory) %>%
  select(doc_id,
         `race/racial/racism/racist`,
         `inequality/inequalities`,
         `discrimination/discriminatory`) %>%
  pivot_longer(-doc_id,
               names_to = "keyword",
               values_to = "n") %>%
  mutate(year = parse_number(doc_id)) %>%
  group_by(keyword) %>%
  mutate(sum = sum(n),
         mean = mean(n),
         cumsum = cumsum(n)) %>%
  mutate(keyword_n = str_c(keyword, " (n = ", sum, ")"))

# make hline for mean
mean_the_word <-
  dfm_keywords_tbl_groups %>%
  distinct(keyword_n, mean)

# make vline for cumsum
cumsum_the_word <-
  dfm_keywords_tbl_groups %>%
  filter(cumsum > sum/2 ) %>%
  group_by(keyword_n) %>%
  filter(cumsum == min(cumsum))

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# count of each keyword groups
# this is the figure included in the manuscript
ggplot(data = dfm_keywords_tbl_groups,
       aes(x = year,
           y = n)) +
  geom_col() +
  facet_wrap( ~ keyword_n,
              ncol = 1,
              scales = "free_y") +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)),
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(name = "Frequency of keyword per year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

ggsave(here::here("analysis/figures/001-keyword-time-series.png"),
       h = 5,
       w = 10)

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

# count all words for each year
all_text_c_summary <-
  summary(all_text_c) %>%
  mutate(year = parse_number(Text))

# compute proportion of all words per year and sum up the keywords
dfm_keywords_tbl_prop <-
  dfm_keywords_tbl %>%
  left_join(all_text_c_summary) %>%
  mutate(prop = n / Tokens ) %>%
  group_by(keyword) %>%
  mutate(sum_the_word = sum(n)) %>%
  mutate(keyword_n = str_c(keyword, " (n = ", sum_the_word, ")"))

# plot of keywords as a proportion of all words per year
#
ggplot(data = dfm_keywords_tbl_prop,
       aes(x = year ,
           y = prop)) +
  geom_col() +
  facet_wrap( ~ keyword,
              ncol = 1,
              scales = "free_y") +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)),
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(name = "Proportion of all words per year",
                     labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))
#-----------------------------------------------------------------------
