library(quanteda)
library(tidyverse)

# https://quanteda.io/reference/textstat_collocations.html
if(!exists("all_text_clean")){
  all_text_clean <- readRDS(here::here("analysis","data", "all_text_clean.rds"))
}

names_all_text_clean <- names(all_text_clean)

# count all words for each year
if(!exists("all_text_c")){
  all_text_c <- corpus(all_text_clean)
}

# tokenize

  toks <- tokens(all_text_c,
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE)

# what are the tfidf values overall,
# so we can compare to the values in the kwik
toks_dfm <-
toks %>%
  dfm(remove = stopwords(),
      verbose = TRUE,
      remove_punct = TRUE) %>%
  dfm_tfidf(
    scheme_tf = "count",
    scheme_df = "inverse",
    force = FALSE
  )

#character vector: separating words by whitespaces and wrap the vector by phrase()
multiword <-    c("race",
                  "racism",
                  "racial",
                  "racist")

#keyword - in- context
kwic_output <- kwic(toks, pattern = phrase(multiword))
# head(kwic(toks, pattern = phrase(multiword)))

## Explore some visualization methods
kwic_output %>%
  filter(keyword == "race")

kwic_output_tidy <-
kwic_output %>%
  group_nest(keyword)

# prepare to plot

# store output
kwic_plots <- vector("list", length = length(multiword))
pre_post_tfidf <- vector("list", length = length(multiword))
x_pre <- vector("list", length = length(multiword))
x_post <- vector("list", length = length(multiword))

for(i in 1:length(multiword)){
keyword <- i

keyword_n <- kwic_output_tidy$keyword[keyword]

#----------------------------------------------
# tf-idf weighting

x_pre[[i]] <-
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
  tibble(featnames = names(x_pre[[i]]@x),
         tfidf = x_pre[[i]]@x) %>%
  group_by(featnames) %>%
  summarise(sum_tfidf = sum(tfidf, na.rm = TRUE)) %>%
  arrange(desc(sum_tfidf)) %>%
  slice(1:15) %>%
  rownames_to_column()


x_post[[i]] <-
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
  tibble(featnames = names(x_post[[i]]@x),
         tfidf = x_post[[i]]@x) %>%
  group_by(featnames) %>%
  summarise(sum_tfidf = sum(tfidf, na.rm = TRUE)) %>%
  arrange(desc(sum_tfidf)) %>%
  slice(1:15) %>%
  rownames_to_column()

pre_post_tfidf[[i]] <- bind_rows(pre = x_tbl_pre,
                            post = x_tbl_post,
                            .id = "pos") %>%
  mutate(rank = parse_number(rowname)) %>%
  select(-rowname)

kwic_plots[[i]] <-
ggplot() +
  geom_text(data = pre_post_tfidf[[i]] %>% filter(pos == "pre"),
            aes(x  = 2,
                y = rank,
                label = featnames,
                size  = sum_tfidf)
            ) +
  geom_text(data = pre_post_tfidf[[i]] %>% filter(pos == "post"),
            aes(x  = 8,
                y = rank,
                label = featnames,
                size  = sum_tfidf)
  ) +
  geom_text(data = tibble(keyword_n = keyword_n),
            aes(x  = 5,
                y = 8,
                label = keyword_n),
                size  = 10,
            colour = "red") +
  scale_y_reverse() +
  scale_size(range = c(6, 10)) +
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
p_4 <- plot_grid(plotlist = kwic_plots, nrow = 1)

pngfile_4 <- here::here("analysis/figures/004-keyword-kwic.png")
jpgfile_4 <- here::here("analysis/figures/004-keyword-kwic.jpg")

library(ragg)

# write PNG file with desired size and resolution
agg_png(pngfile_4,
        width = 13,
        height = 5,
        units = "cm",
        res = 1000,
        scaling = 0.25)
p_4

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in_4 <- image_read(pngfile_4)
png_4_jpg_4 <- image_convert(img_in_4, "jpg")
image_write(png_4_jpg_4, jpgfile_4, density = 1000, quality = 100)

# check distributions of tfidf
# histogram of all tfidf values
# keywords_tfidf_hist_plot <-
# map(pre_post_tfidf, ~.x %>% select(sum_tfidf) %>% pull) %>% unlist %>%
# tibble(x = .) %>%
#   ggplot() +
#   aes(x) +
#   geom_histogram() +
#   scale_x_log10()
# pre_post_tfidf
#
# # histogram of all tfidf values
# allwords_tfidf_hist_plot <-
# tibble(x = as.vector(toks_dfm)) %>%
#   ggplot() +
#   aes(x) +
#   geom_histogram() +
#   scale_x_log10()
#
# plot_grid(keywords_tfidf_hist_plot,
#           allwords_tfidf_hist_plot,
#           nrow = 2)


# look into 'race and x' to see about broader social issues
toks_ngram <- tokens_ngrams(toks, n = 3)
head(toks_ngram[[1]], 30)

race_and_bigrams <-
map(toks_ngram,
    ~str_subset(.x, "^race_and"))

unlist(race_and_bigrams) %>%
  tibble(bigram = .) %>%
  group_by(bigram) %>%
  tally(sort = TRUE)

# race and class most frequent, six times, all after 2004




