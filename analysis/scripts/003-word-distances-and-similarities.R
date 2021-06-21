
word_similarities_plots_fn <- function(){

library(tidyverse)
library(quanteda)
library(quanteda.textstats)

if(!exists("all_text_clean")){
  all_text_clean <- readRDS(here::here("analysis","data", "all_text_clean.rds"))
}

# split every n words -------------------------------------------
# smaller 'documents' gives more distinctive topics, ideally it would be
# one document per abstract, but we would need to manually mark each abstract

chunk_size <- 5000
word_count_by_year <-
  map(all_text_clean,
      str_count)

all_years_text_in_chuncks <-
  map(all_text_clean,
      ~split(str_split(.x, " ")[[1]],
             ceiling(seq_along(str_split(.x, " ")[[1]])/ chunk_size )))

library(purrr)
all_years_text_in_chuncks_flat <- flatten(all_years_text_in_chuncks)

# get names for chunks that include years
chunks_per_year <-
  map(all_years_text_in_chuncks, length)

chunk_names <-
  map2(chunks_per_year,
       names(chunks_per_year),
       ~rep(.y, .x)) %>%
  unlist() %>%
  unname() %>%
  make.unique()

names(all_years_text_in_chuncks_flat) <- chunk_names

all_years_text_in_chuncks_flat2  <-
  map(all_years_text_in_chuncks_flat,
      ~paste0(.x, collapse = " "))

all_years_text_in_chuncks_flat3 <- unlist(all_years_text_in_chuncks_flat2)

all_text_clean_chunks <- all_years_text_in_chuncks_flat3

# end split -----------------------------------------------

# count all words for each year
all_text_c <- corpus(all_text_clean_chunks)

# don't remove any stopwords
all_text_c_dtm_no_stop <-
  all_text_c %>%
  tokens(
    verbose = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    split_hyphens = TRUE,
    remove_punct = TRUE) %>%
  tokens_select(min_nchar = 3) %>%
  dfm() %>%
  dfm_select(pattern = c("81st", "contexto", "arqueológicas"),
             selection = "remove")

# Explore key words over time
keywords <-
  c("race",
    "racism",
    "racial",
    "racist")

dfm_keywords <-
  dfm_select(all_text_c_dtm_no_stop,
             pattern = keywords,
             selection = "keep")

# similarities for specific documents for keywords

# compute similarities between features using relative frequency
simi_all <-
  all_text_c_dtm_no_stop %>%
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
p_3 <- ggplot(list_simil_tbl) +
  aes( kw, id) +
  geom_text(aes(label = feat,
                      size = corval,
                colour = corval)) +
  scale_colour_gradient(low = "blue",
                        high = "red",
                        name = "Similarity") +
  scale_y_reverse() +
  scale_size_continuous(range = c(5, 9)) +
  scale_x_discrete(position = "top") +
  theme_classic() +
  theme(axis.text.x.top  = element_text(size = 20)) +
  labs(x = "",
       y = "rank") +
  guides(size = FALSE)

pngfile_3 <- here::here("analysis/figures/003-keyword-similar-words.png")
jpgfile_3 <- here::here("analysis/figures/003-keyword-similar-words.jpg")

library(ragg)

# write PNG file with desired size and resolution
agg_png(pngfile_3,
        width = 8,
        height = 5,
        units = "cm",
        res = 1000,
        scaling = 0.3)

print(p_3)

invisible(dev.off())

# convert PNG to JPG
library(magick)
img_in_3 <- image_read(pngfile_3)
png_3_jpg_3 <- image_convert(img_in_3, "jpg")
image_write(png_3_jpg_3, jpgfile_3, density = 1000, quality = 100)


}



