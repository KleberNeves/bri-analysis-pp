p_load(tidyverse, readxl, stringr, readxl, data.table, openxlsx)

options(dplyr.summarise.inform = FALSE)

source("functions_summarizer.R")

make_merged_summary = function (data_fns, exp_code, method, labunit) {
  print(exp_code)

  rep_data = map_dfr(data_fns, read_tsv, show_col_types = F)
  
  rep_data = rep_data |>
    group_by(LAB) |>
    mutate(
      Group2_Perc = Group2_Perc / mean(Group1_Perc, na.rm = T),
      Group1_Perc = Group1_Perc / mean(Group1_Perc, na.rm = T)
    ) |>
    ungroup() |>
    mutate(
      LAB = "All LABs"
    )
  
  export_file(rep_data, exp_code, "LAB00", method, S = ifelse(is.na(labunit), "", "LABUNIT"))
  
}

# Dictionary mapping experiments to data files
file_dict = rbind(
  read_tsv("./replication-results/PCR_results_dict.tsv", show_col_types = F),
  read_tsv("./replication-results/PCR_ALT_results_dict.tsv", show_col_types = F),
  read_tsv("./replication-results/EPM_results_dict.tsv", show_col_types = F),
  read_tsv("./replication-results/MTT_results_dict.tsv", show_col_types = F),
  read_tsv("./replication-results/MTT_ALT_results_dict.tsv", show_col_types = F)
) |>
  filter(
    !is.na(Filename),
    # Excluding secondary outcomes from EPM experiments
    !str_detect(Filename, "secondary"),
  ) |>
  mutate(
    method = str_extract(Filename, "results[/].+[/]") |> str_remove_all("(results|primary_outcomes)") |> str_remove_all("[/]")
  )


# For every set of experiments, merge into single file (the "big experiment")

file_to_exp_dict = tibble(Filename = character(0), LAB = character(0), EXP = character(0))

file_dict |>
  group_by(EXP, method, UNIT) |>
  group_walk(
    ~ make_merged_summary(.x$Filename, unique(.y$EXP), unique(.y$method), unique(.y$UNIT))
  )

write_tsv(file_to_exp_dict, "replication-results/BIG_EXP_results_dict.tsv")