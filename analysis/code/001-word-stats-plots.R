library(tidyverse)
library(quanteda)

# read in all txt files of SAA abstracts from 1962 to 2020. Data is from SAA website and scanned images of text is converted to PDF using OCR
# the code for OCR is here: https://github.com/benmarwick/saa-meeting-abstracts/blob/master/code/001-PDF-page-images-to-txt.R
# this is a character vector, one abstract per element
# can skip code between line 12 and line 24 (skip the code before that line to save time)

all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))

# count all words for each year (skip this and run line 143)
all_text_c <- corpus(all_text)
all_txts_c_summary <-
  summary(all_text_c) %>%
  mutate(year = parse_number(Text))

# make a dfm (skip this, and run line 26)
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
word_three_groups <-
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

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# load dfs of all text and abstract and join
all_txts_c_summary <-
  readRDS(here::here("analysis","data", "all_text_c_summary.rds"))
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
  mutate(max_in_class = ifelse(prop == max(prop), n, "")) %>%
  mutate(keyword_n = str_c(keyword, " (n = ", sum,")")) %>%
  mutate(keyword_sets = case_when(
    sum == 262 ~ "race/racial/racism/racist (n = 262, maximum of 38 in 2018)",
    sum == 934 ~ "inequality/inequalities (n = 934, maximum of 110 in 2016)",
    sum == 87 ~ "discrimination/discriminatory (n = 87, maximum of 4 in 1977)",
    TRUE ~ "other"))

# plot of keywords as a proportion of all words per year
# this is the figure included in the manuscript
keyword_proportion_per_year <-
  ggplot(data = dfm_keywords_tbl_prop,
         aes(x = year ,
             y = prop)) +
  geom_col() +
  facet_wrap( ~ keyword_sets,
              ncol = 1,
              scales = "free_y") +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)),
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(name = "Proportion of keywords per year",
                     labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5),
        strip.text = element_text(size = 12))

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

all_txts_c_summary_join_abstract <-
  all_txts_c_summary %>%
  left_join(saa_abstract) %>%
  select(Text, Tokens, number_of_abstracts, year) %>%
  mutate(`word/abstract` = Tokens/number_of_abstracts)

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
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

# all word per abstract
all_words_per_abstracts_per_year <-
  ggplot(data = all_txts_c_summary_join_abstract,
         aes(x = year,
             y = `word/abstract`)) +
  geom_col() +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)),
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(labels = c(seq(0, 180, 50)),
                     breaks = seq(0, 180, 50),
                     name = "word/abstract") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

# combine three plots
library(cowplot)

plot_grid(all_words_per_year,
          all_words_per_abstracts_per_year,
          keyword_proportion_per_year,
          rel_heights = c(0.5, 0.5, 1.2),
          labels = c('A', 'B', 'C'),
          label_size = 12,
          ncol = 1)

ggsave(here::here("analysis/figures/001-keyword-time-series.png"),
       h = 9,
       w = 9)
