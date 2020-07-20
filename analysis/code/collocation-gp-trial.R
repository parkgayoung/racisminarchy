library(quanteda)

# https://quanteda.io/reference/textstat_collocations.html

all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))

# count all words for each year
all_text_c <- corpus(all_text)

#tokenize
toks <- tokens(all_text_c)

#character vector: separating words by whitespaces and wrap the vector by phrase()
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

<<<<<<< HEAD

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







=======
#select tokens
head(tokens_select(toks, pattern = phrase(multiword)))

#######less useful#######
#compound tokens: joins elements of multi-word expression by underscore.
#e.g. "United_States" I'm not sure whether it's useful for our research though
comp_toks <- tokens_compound(toks, pattern = phrase(multiword))
head(tokens_select(comp_toks, pattern = "racism_inequality"))

#dictionary: elements of multi-word expressions should be separately by whitespaces
#I don't know what can be the multi-word for out research
#multiword_dict <- dictionary(list(country = "United States",
#                                  city = "New York"))
###########################

#discover collocations

col <- toks %>%
  tokens_remove(stopwords("en")) %>%
  tokens_select(pattern = "^[A-Z]", valuetype = "regex",
                case_insensitive = FALSE, padding = TRUE) %>%
  textstat_collocations(min_count = 5, tolower = FALSE)

head(col)

# compound collocation : collocations for multi words

comp_toks2 <- tokens_compound(toks, pattern = col)
head(kwic(comp_toks2, pattern = "racism_inequality"))
>>>>>>> d6ac873d63077efdf34b48e7abd8afef6a81448d

