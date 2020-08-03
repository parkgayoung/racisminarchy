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

# count for groupings
ggplot(data = dfm_keywords_tbl_groups,
       aes(x = year,
           y = n)) +
  geom_col() +
  geom_vline(data = cumsum_the_word,
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

# distances for specific documents for keyword
dist_keywords <- textstat_dist(dfm_keywords,
                               dfm_keywords[,"discrimination"],
                               margin = "documents")

dist_keywords_tbl <-
  as.data.frame(dist_keywords, to = "data.frame")

# compute similarities between features using relative frequency
simi_all <-
  dfm_weight(all_text_c_dtm, scheme = "prop") %>%
  textstat_simil(selection = keywords,
                 method = "correlation",
                 margin = "features")

head(as.matrix(simi_all), 10)
as.list(simi_all, n = 10)

# compute similarities between features
simi_keywords <-
  dfm_weight(dfm_keywords, scheme = "prop") %>%
  textstat_simil(selection = dfm_keywords,
                                all_text_c_dtm[, c("black",
                                                   "people",
                                                   "african",
                                                   "asian")],
                                method = "cosine",
                                margin = "features")
head(as.matrix(simi_keywords), 10)

# read data from googlesheet
library(googlesheets4)
event <- read_sheet("https://docs.google.com/spreadsheets/d/1DXgcXOsD_1yLfJ9tyADZhQr3rOK6Y1yL_GDuGyYtUNs/edit?ts=5ef3e46a#gid=0")

# tidy up
event_tally <-
  event %>%
  group_by(`Start year`) %>%
  count(`Start year`) %>%
  filter(!is.na(`Start year`),
         `Start year` > 1961) %>%
  ungroup()

n_major_events <- sum(event_tally$n, na.rm = TRUE )

# visualize
major_event <-
  ggplot(event_tally) +
  aes(`Start year`) +
  geom_histogram() +
  theme_minimal() +
  labs(x = "Year",
       y = paste0("African-American\nhistorical major event (n = ", n_major_events, ")")
  )

# saa word per year
saa_words_per_year <-
  tibble(year = as.numeric(names(rowSums(dfm_keywords))),
         saa_wordcount = rowSums(dfm_keywords))

# join SAA and history data
saa_and_event_tbl <-
  event_tally %>%
  right_join(saa_words_per_year, by = c(`Start year` = "year")) %>%
  arrange(`Start year`) %>%
  replace_na(list(n = 0, saa_wordcount = 0)) %>%
  mutate(
    `Same year` = saa_wordcount,
    `1 year lag` = lag(saa_wordcount),
    `2 year lag` = lag(saa_wordcount, 2),
    `3 year lag` = lag(saa_wordcount, 3),
    `4 year lag` = lag(saa_wordcount, 4),
    `5 year lag` = lag(saa_wordcount, 5),
    `6 year lag` = lag(saa_wordcount, 6)) %>%
  select(-saa_wordcount,
         -`Same year`)  %>%
  pivot_longer(-c(`Start year`, n)) %>%
  mutate(name = factor(name,
                       levels = c(
                         "1 year lag",
                         "2 year lag",
                         "3 year lag",
                         "4 year lag",
                         "5 year lag",
                         "6 year lag")))

# Major African-American events on the scatterplot
n_major_events_subset <-
  saa_and_event_tbl %>%
  distinct(`Start year`, n) %>%
  pull(n) %>%
  sum()

# scatter plot
plot <-
  ggplot(saa_and_event_tbl,
         aes(n, value)) +
  geom_text(aes(label = `Start year`),
            size = 2) +
  geom_smooth(se = FALSE,
              method = "lm") +
  stat_cor(label.y = 120,
           label.x = 0.3,
           size = 3) +
  stat_regline_equation(label.y = 110,
                        label.x = 0.3,
                        size = 3) +
  theme_bw(base_size = 10) +
  labs(x = paste0("African-American major event frequency (n = ", n_major_events, ")"),
       y = paste0("Mentions of 'race', etc. (n = ", n_words, ")\nin SAA abstracts (n = ", n_abstracts, ")")) +
  facet_wrap( ~ name)

