
ha_get_abstracts_fn <- function(){

library(tidyverse)
library(rvest)

#---------------------------
# web scrape HA journal

page_of_list_of_papers <-
  "https://link.springer.com/journal/41636/volumes-and-issues"

library(rvest)

links_of_all_issues <-
  page_of_list_of_papers %>%
  read_html()  %>%
  html_elements(".u-interface-link") %>%
  html_attr("href")

links_of_papers_in_all_issues <-
  map(links_of_all_issues,
      ~.x %>%
        paste0("https://link.springer.com", .) %>%
        read_html()  %>%
        html_elements(".c-card__title a") %>%
        html_attr("href")
  )

# get titles & abstracts: "#Abs1-content p , .c-article-title"
# this takes a long time
ha_titles_abstracts <-
  map(unlist(links_of_papers_in_all_issues),
      ~.x %>%
        read_html()  %>%
        html_elements("#Abs1-content p , .c-article-title") %>%
        html_text()
  )


# get years ".c-article-info-details span"
# this takes a long time
ha_years <-
  map(unlist(links_of_papers_in_all_issues),
      ~.x %>%
        read_html()  %>%
        html_elements(".c-article-info-details span") %>%
        html_text()
  )

# years as vector
ha_years_vtr <-
  map_dbl(ha_years, ~parse_number(.x[3]))

# title and abstract as data frame
ha_titles_abstracts_df <-
  map_df(ha_titles_abstracts,
         ~tibble(title = .x[1],
                 abstract = .x[2])
  )

# combine years, titles and abstracts into a data frame
ha_titles_abstracts_years_df <-
  tibble(year = ha_years_vtr,
         title = ha_titles_abstracts_df$title,
         abstract = ha_titles_abstracts_df$abstract) %>%
  drop_na()

# export as CSV
write_csv(ha_titles_abstracts_years_df,
          here::here("analysis/data/ha_titles_abstracts_years_df.csv"))

}

