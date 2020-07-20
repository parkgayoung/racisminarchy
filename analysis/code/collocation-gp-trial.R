library(quanteda)

# https://quanteda.io/reference/textstat_collocations.html

all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))

# count all words for each year
all_text_c <- corpus(all_text)

#tokenize
toks <- tokens(all_text_c)

#character vector: The most basic way to define multi-word expressions
multiword <-  c("race",
                "racism",
                "racist",
                "racial",
                "discrimination",
                "inequality",
                "inequalities")

#keyword - in- context
kwic_output <- kwic(toks, pattern = phrase(multiword))
head(kwic(toks, pattern = phrase(multiword)))


## Explore some visualization methods

kwic_output %>%
  filter(keyword == "race")


library(tidyverse)
kwic_output_tidy <-
kwic_output %>%
  group_nest(keyword)

# prepare to plot

# choose a number
keyword <- 7

keyword_n <- kwic_output_tidy$keyword[keyword]

#----------------------------------------------
# tf-idf weighting

x_pre <-
kwic_output_tidy$data[[keyword]]$pre %>%
  dfm(remove = stopwords(),
      verbose = TRUE,
      remove_punct = TRUE) %>%
dfm_tfidf(
  scheme_tf = "count",
  scheme_df = "inverse",
  force = FALSE
)

x_tbl_pre <-
  tibble(featnames = names(x_pre@x),
         tfidf = x_pre@x) %>%
  group_by(featnames) %>%
  summarise(sum_tfidf = sum(tfidf, na.rm = TRUE)) %>%
  arrange(desc(sum_tfidf)) %>%
  slice(1:15) %>%
  rownames_to_column()


x_post <-
  kwic_output_tidy$data[[keyword]]$post %>%
  dfm(remove = stopwords(),
      verbose = TRUE,
      remove_punct = TRUE) %>%
  dfm_tfidf(
    scheme_tf = "count",
    scheme_df = "inverse",
    force = FALSE
  )

x_tbl_post <-
  tibble(featnames = names(x_post@x),
         tfidf = x_post@x) %>%
  group_by(featnames) %>%
  summarise(sum_tfidf = sum(tfidf, na.rm = TRUE)) %>%
  arrange(desc(sum_tfidf)) %>%
  slice(1:15) %>%
  rownames_to_column()

pre_post_tfidf <- bind_rows(pre = x_tbl_pre,
                            post = x_tbl_post,
                            .id = "pos") %>%
  mutate(rank = parse_number(rowname)) %>%
  select(-rowname)

ggplot() +
  geom_text(data = pre_post_tfidf %>% filter(pos == "pre"),
            aes(x  = 2,
                y = rank,
                label = featnames,
                size  = sum_tfidf)
            ) +
  geom_text(data = pre_post_tfidf %>% filter(pos == "post"),
            aes(x  = 8,
                y = rank,
                label = featnames,
                size  = sum_tfidf)
  ) +
  geom_text(data = tibble(keyword_n = keyword_n),
            aes(x  = 5,
                y = 8,
                label = keyword_n,
                size  = 5)
  ) +
  scale_y_reverse() +
  scale_size(range = c(3, 7)) +
  coord_cartesian(xlim = c(0, 10)) +
  theme_bw()

#-----------------------------------------------------

x_pre <-
  kwic_output_tidy$data[[1]]$pre %>%
  dfm(remove = stopwords(),
      verbose = TRUE,
      remove_punct = TRUE) %>%
  dfm_weight("count") %>%
  colSums() %>%
  stack %>%
  arrange(desc( values )) %>%
  slice(1:15) %>%
  rownames_to_column()


x_post <-
  kwic_output_tidy$data[[1]]$post %>%
  dfm(remove = stopwords(),
      verbose = TRUE,
      remove_punct = TRUE)  %>%
  dfm_weight("count") %>%
  colSums() %>%
  stack %>%
  arrange(desc( values )) %>%
  slice(1:15) %>%
  rownames_to_column()

pre_post_tfidf <- bind_rows(pre = x_pre,
                            post = x_post,
                            .id = "pos") %>%
  mutate(rank = parse_number(rowname)) %>%
  select(-rowname)

ggplot() +
  geom_text(data = pre_post_tfidf %>% filter(pos == "pre"),
            aes(x  = 2,
                y = rank,
                label = ind,
                size  = values)
  ) +
  geom_text(data = pre_post_tfidf %>% filter(pos == "post"),
            aes(x  = 8,
                y = rank,
                label = ind,
                size  = values)
  ) +
  geom_text(data = tibble(keyword_n = keyword_n),
            aes(x  = 5,
                y = 8,
                label = keyword_n,
                size  = 5)
  ) +
  scale_y_reverse() +
  scale_size(range = c(3, 7)) +
  coord_cartesian(xlim = c(0, 10)) +
  theme_void()








