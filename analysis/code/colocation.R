# https://quanteda.io/reference/textstat_collocations.html

all_text <- readRDS(here::here("analysis","data", "saa_abstracts.rds"))

# count all words for each year
all_text_c <- corpus(all_text)

colo <-
textstat_collocations(
  all_text_c,
  method = "lambda",
  size = 2,
  min_count = 2,
  smoothing = 0.5,
  tolower = TRUE
)

# then filter so we only see the results for our keywords...

colo  %>%
  filter(str_detect(collocation, " race"))

# see https://tm4ss.github.io/docs/Tutorial_4_Term_extraction.html for
# more ideas

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

