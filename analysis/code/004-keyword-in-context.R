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

kwic_plots <- vector("list", length = length(multiword))

for(i in 1:length(multiword)){
keyword <- i

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

kwic_plots[[i]] <-
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
                label = keyword_n),
                size  = 4,
            colour = "red") +
  scale_y_reverse() +
  scale_size(range = c(3, 7)) +
  guides(size = FALSE) +
  coord_cartesian(xlim = c(0, 10)) +
  # coord_cartesian(ylim = c(10, 0)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

}

library(cowplot)
plot_grid(plotlist = kwic_plots, nrow = 2)

ggsave(here::here("analysis/figures/003-keyword-kwik.png"),
       h = 7,
       w = 17)

#-----------------------------------------------------
