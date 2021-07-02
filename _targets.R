# load libraries we need here
library(targets)
library(tarchetypes) # for tar_render

# load the functions from our script files
my_script_files <- list.files(here::here("analysis/scripts"),
                              pattern = ".R$",
                              full.names = TRUE)
purrr::walk(my_script_files, source)

# load libraries used in all functions
tar_option_set(packages = c("tidyverse", "quanteda"))

list(
    # prepares the SAA data ready for analysis
    # takes a long time
    tar_target(prepare_the_data,        # name of target
               prepare_the_data_fn()),  # name of function to run

    # prepares the HA data ready for analysis
    # takes a long time
    tar_target(ha_get_abstracts,        # name of target
               ha_get_abstracts_fn()),  # name of function to run

    # makes Fig 1: corpus level, keyword prop over time
    tar_target(word_stats_plots,        # name of target
               word_stats_plots_fn()),  # name of function to run

    # makes Fig 2: corpus level, main topics and change over time
    # takes a long time
    tar_target(topic_model_plots,      # name of target
               make_topic_model_fn()), # name of function to run

    # makes Fig 3: document level, word similarities
    tar_target(word_similarities_plots,       # name of target
               word_similarities_plots_fn()), # name of function to run

    # makes Fig 4: sentence level, kwic plot
    tar_target(kwic_plot,       # name of target
               kwic_plot_fn()), # name of function to run

    # makes Fig 5: compare with historical events
    tar_target(social_event_correlation_plots,       # name of target
               social_event_correlation_plots_fn()), # name of function to run

    # makes Fig 6: discussion, compare HA and SAA, we use output from earlier target
    tar_target(ha_saa_compare_plot,                       # name of target
               ha_saa_compare_plot_fn(word_stats_plots)), # name of function to run

    # makes some linear model stats that we use in the text
    tar_target(ha_social_events_stats,     # name of target
               ha_social_events_stats_fn()),  # name of function to run

    # knit Rmd to produce word doc of manuscript
    tarchetypes::tar_render(paper,
                            here::here("analysis/paper/paper.Rmd"))

    # end of the targets
    # run
    # targets::tar_make()
    # to run all code and knit Rmd file to Word docx
)
