library(quanteda)
library(tidyverse)


# https://quanteda.io/reference/textstat_collocations.html

# run 002 to chunk first

race_words <- c(
  "race", # because "race to stay ahead" etc.
  "racism",
  "racial",
  "racist")
all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))
names_all_text <- names(all_text)

# split every n words -------------------------------------------
# smaller 'documents' gives more distinctive topics, ideally it would be
# one document per abstract, but we would need to manually mark each abstract

chunk_size <- 5000
word_count_by_year <-
  map(all_text,
      str_count)

all_years_text_in_chuncks <-
  map(all_text,
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

all_text <- all_years_text_in_chuncks_flat3


# end split -----------------------------------------------

all_text_c <- corpus(all_text)



#tokenize
if(!exists("toks")){
  toks <- tokens(all_text_c)
}

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
head(kwic(toks, pattern = phrase(multiword)))

#character vector: separating words by whitespaces and wrap the vector by phrase()
figure3_keywords <-    c("injuries",
                         "civil",
                         "permitted",
                         "ontologies",
                         "sovereignty",
                         "prevalence",
                         "ethnology",
                         "individual")

explore_word <- c("eastern")

figure3_kwic_output <- kwic(toks, pattern = phrase(figure3_keywords), window = 200)
figure3_kwic_output <- kwic(toks, pattern = phrase(explore_word), window = 200)

