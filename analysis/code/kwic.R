
# Keyword in context

all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))

# count all words for each year
library(quanteda)
all_text_c <- corpus(all_text)


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

