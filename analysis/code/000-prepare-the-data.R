library(tidyverse)
library(quanteda)

# read in all txt files of SAA abstracts from 1962 to 2020. Data is from SAA website and scanned images of text is converted to PDF using OCR
# the code for OCR is here: https://github.com/benmarwick/saa-meeting-abstracts/blob/master/code/001-PDF-page-images-to-txt.R
# this is a character vector, one abstract per element

all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))

# we want to remove the phrase “individual abstracts of the” from everything because
# it's a header on the program during 2017-2020

all_text_clean <-
  map_chr(all_text,
          ~str_remove_all(.x,  "individual abstracts of the") )

# count all words for each year
all_text_c <- corpus(all_text_clean)

# make a dfm
all_text_c_dtm <-
  all_text_c %>%
  tokens(verbose = TRUE,
         remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE,
         remove_separators = TRUE,
         split_hyphens = FALSE) %>%
  dfm() %>%
  dfm_remove(pattern = stopwords("english"))

# save it for reuse
saveRDS(all_text_c_dtm,
        here::here("analysis/data/all_text_c_dtm.rds"))

# for words per year, quite slow, takes a few minutes...
all_txts_c_summary <-
  summary(all_text_c) %>%
  mutate(year = parse_number(Text))

# save it for reuse
saveRDS(all_txts_c_summary,
        here::here("analysis/data/all_txts_c_summary.rds"))




