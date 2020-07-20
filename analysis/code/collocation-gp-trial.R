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
                "racial",
                "discrimination",
                "inequality",
                "inequalities")

#keyword - in- context
head(kwic(toks, pattern = phrase(multiword)))
