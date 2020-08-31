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


