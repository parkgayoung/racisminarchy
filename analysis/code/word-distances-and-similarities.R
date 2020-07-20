library(tidyverse)
library(quanteda)

all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))
names_all_text <- names(all_text)

# count all words for each year
all_text_c <- corpus(all_text)

# don't remove any stopwords
all_text_c_dtm <-
  dfm(all_text_c,
      tolower = TRUE,
      verbose = TRUE,
      remove_numbers = TRUE,
      remove_symbols = TRUE,
      split_hyphens = TRUE,
      remove_punct = TRUE)


# Explore key words over time
keywords <-
  c("race",
    "racism",
    "racial",
    "discrimination",
    "inequality",
    "inequalities")

dfm_keywords <-
  dfm_select(all_text_c_dtm,
             pattern = keywords,
             selection = "keep")

# similarities for specific documents for keywords

# compute similarities between features using relative frequency
simi_all <-
  all_text_c_dtm %>%
  dfm_select(min_nchar = 4) %>%
  dfm_trim(min_termfreq = 100,
           min_docfreq = 5) %>%
  dfm_weight(scheme = "prop") %>%
  textstat_simil(y = dfm_keywords,
                 method = "correlation",
                 margin = "features")

# tidy
list_simil <- as.list(simi_all, n = 20)

list_simil

list_simil_lst <-
  map(list_simil, ~tibble(corval = .x, feat = names(.x)))

list_simil_tbl <-
  bind_rows(list_simil_lst, .id = "kw") %>%
  group_by(kw) %>%
  mutate(id = row_number())

# plot
library(ggrepel)
ggplot(list_simil_tbl) +
  aes( kw, id) +
  geom_text(aes(label = feat,
                      size = corval,
                colour = corval)) +
  scale_colour_gradient(low = "blue",
                        high = "red",
                        name = "Correlation") +
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  theme_classic() +
  theme(axis.text.x.top  = element_text(size = 20)) +
  labs(x = "",
       y = "rank") +
  guides(size = FALSE)



## spare -----------------------------
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
