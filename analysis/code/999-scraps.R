

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

# head(as.matrix(simi_all), 10)
# as.list(simi_all, n = 10)

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
# head(as.matrix(simi_keywords), 10)

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

##-----------------------------------------------------------
# from kwik.R
# Keyword in context

all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))

# count all words for each year
library(quanteda)
# count all words for each year
if(!exists("all_text_c")){
  all_text_c <- corpus(all_text)
}



# Explore key words over time
keywords <-
  c("race",
    "racism",
    "discrimination",
    "inequality",
    "inequalities")

kwik_output <-
  kwic(
    all_text_c,
    pattern = keywords,
    window = 5,
    separator = " ",
    case_insensitive = TRUE
  )

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

#----------------------------------------------------

library(quanteda)

# https://quanteda.io/reference/textstat_collocations.html

if(!exists("all_text")){
  all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))
}

# count all words for each year
if(!exists("all_text_c")){
  all_text_c <- corpus(all_text)
}

#tokenize
if(!exists("toks")){
  toks <- tokens(all_text_c)
}


#character vector: separating words by whitespaces and wrap the vector by phrase()
multiword <-    c("race",
                  "racism",
                  "racial",
                  "racist",
                  "inequality",
                  "inequalities",
                  "discrimination",
                  "discriminatory")

#keyword - in- context
kwic_output <- kwic(toks, pattern = phrase(multiword))
head(kwic(toks, pattern = phrase(multiword)))

## Explore some visualization methods
library(tidyverse)
kwic_output %>%
  filter(keyword == "race")

kwic_output_tidy <-
  kwic_output %>%
  group_nest(keyword)

# prepare to plot

# choose a number
keyword <- 6

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
# from collocation-gp-trial.R

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

# from 004-colocation.R -----------------------------------------------

library(quanteda)

# https://quanteda.io/reference/textstat_collocations.html

if(!exists("all_text")){
  all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))
}

# count all words for each year
if(!exists("all_text_c")){
  all_text_c <- corpus(all_text)
}

# takes a long time!
if(!exists("colo")){
  colo <-
    textstat_collocations(
      all_text_c,
      method = "lambda",
      size = 2,
      min_count = 2,
      smoothing = 0.5,
      tolower = TRUE
    )
}

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


library(tidyverse)

# then filter so we only see the results for our keywords...
# This code takes long time..

colo_tbl <-
  colo  %>%
  as_tibble() %>%
  separate(collocation, into = c("one", "two"), sep = " ") %>%
  filter(str_detect(one, keywords)) %>%
  filter(!one %in% c("terrace", "terraces", "traces", "trace", "traced", "racemization", stopwords())) %>%
  filter(!two %in% stopwords())

library(ggraph)
library(tidygraph)

# Create graph of highschool friendships
graph <- as_tbl_graph(highschool) %>%
  mutate(Popularity = centrality_degree(mode = 'in'))

graph <- as_tbl_graph(
  colo_tbl %>%
    rename(from = one,
           to = two) %>%
    select(to, from, count)
)

ggraph(graph, layout = 'kk') +
  geom_edge_link() +
  geom_node_label(aes(size = centrality_pagerank(),
                      label = name)) +
  theme(legend.position = 'bottom')

# see https://tm4ss.github.io/docs/Tutorial_4_Term_extraction.html for
# more ideas









































