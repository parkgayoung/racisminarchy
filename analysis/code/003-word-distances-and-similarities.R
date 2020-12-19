library(tidyverse)
library(quanteda)

if(!exists("all_text")){
  all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))
}

names_all_text <- names(all_text)

# count all words for each year
if(!exists("all_text_c")){
all_text_c <- corpus(all_text)
}

# don't remove any stopwords
if(!exists("all_text_c_dtm")){
  all_text_c_dtm <-
    dfm(all_text_c,
        tolower = TRUE,
        verbose = TRUE,
        remove_numbers = TRUE,
        remove_symbols = TRUE,
        split_hyphens = TRUE,
        remove_punct = TRUE)

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
                 method = "jaccard",
                 margin = "features")

# tidy
list_simil <- as.list(simi_all, n = 20)

# list_simil

list_simil_lst <-
  map(list_simil, ~tibble(corval = .x, feat = names(.x)))

list_simil_tbl <-
  bind_rows(list_simil_lst, .id = "kw") %>%
  group_by(kw) %>%
  mutate(id = row_number())

# plot

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# this is the figure included in the manuscript
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

ggsave(here::here("analysis/figures/003-keyword-similar-words.jpg"),
       h = 4,
       w = 10,
       scale = 4,
       units = "cm",
       dpi = "retina")

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------


