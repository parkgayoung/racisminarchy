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
                "racial",
                "discrimination",
                "inequality",
                "inequalities")

#keyword - in- context
head(kwic(toks, pattern = phrase(multiword)))

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

