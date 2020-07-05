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

dfm_keywords_tbl <-
  convert(dfm_keywords, to = "data.frame") %>%
  pivot_longer(-document,
               names_to = "keyword",
               values_to = "n") %>%
  mutate(year = parse_number(document))

# plot of frequency of keywords per year
ggplot(data = dfm_keywords_tbl,
       aes(x = year ,
           y = n)) +
  geom_col() +
  facet_wrap( ~ keyword,
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
simi_keywords <- textstat_simil(dfm_keywords,
                                all_text_c_dtm[, c("black",
                                                   "people",
                                                   "african",
                                                   "asian")],
                                method = "correlation",
                                margin = "features")
head(as.matrix(simi_keywords), 10)
