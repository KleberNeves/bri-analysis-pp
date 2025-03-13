library(tidyverse)
library(readxl)
library(data.table)
library(openxlsx)

options(dplyr.summarise.inform = FALSE)

source("functions_summarizer.R")

# Reads the complete dataset with all PCR measures
all_data = fread("./gathered-data/PCR_data.csv", dec = ",", na.strings = c("","NA", "Undetermined", "undetermined")) |> as_tibble() |> filter(!is.na(Group)) |> filter(is.na(`Exclude?`))

meta_data = read_delim("./gathered-data/PCR_meta.csv", na = c("","NA"), locale = locale(decimal_mark = ","))

file_to_exp_dict = tibble(Filename = character(0), LAB = character(0), EXP = character(0))

build_summary_single_lab = function(lab_code, exp_data, alternative_pairing, suffix = NULL) {
  exp_code = exp_data$EXP |> unique()
  rep_data = exp_data |> filter(LAB == lab_code)
  
  if (alternative_pairing) {
    is_paired = (meta_data |> filter(EXP == exp_code, LAB == lab_code) |> pull(`Paired Alternative`)) == "Yes"
  } else {
    is_paired = (meta_data |> filter(EXP == exp_code, LAB == lab_code) |> pull(Paired)) == "Yes"
  }
  
  outcome_expression = (meta_data |> filter(EXP == exp_code, LAB == lab_code) |> pull(`Primary Outcome`))
  
  if (outcome_expression == "Optical Density (a.u)") {
    # Replace cases of no amplification with a small value
    zero_replacement_value = min(
      c(
        rep_data$`Band Density Gene of Interest`[rep_data$`Band Density Gene of Interest` != 0],
        rep_data$`Band Density Control Gene`[rep_data$`Band Density Control Gene` != 0]
      ), na.rm = T) / 2
    
    rep_data$`Band Density Gene of Interest`[rep_data$`Band Density Gene of Interest` == 0] = zero_replacement_value
    rep_data$`Band Density Control Gene`[rep_data$`Band Density Control Gene` == 0] = zero_replacement_value
    
    # Change Band densities to be log scale
    rep_data = rep_data |>
      mutate(
        `CT Gene of Interest` = log2(`Band Density Gene of Interest`),
        `CT Control Gene I` = log2(`Band Density Control Gene`),
      )
  } else {
    # Replace cases of no amplification with a small value
    cycles = (meta_data |> filter(EXP == exp_code, LAB == lab_code) |> pull(Cycles))
    
    rep_data$`CT Gene of Interest` = rep_data$`CT Gene of Interest` |> str_replace_all("No amplification", as.character(cycles)) |> str_replace_all(",", ".") |> as.numeric()
  }
  
  # Calculate the primary outcome using delta CTs
  summary_data = rep_data |>
    group_by(ExpUnit, Group) |>
    summarise(
      CT_ControlGene = mean(c(`CT Control Gene I`, `CT Control Gene II`), na.rm = T),
      CT_TargetGene = mean(`CT Gene of Interest`, na.rm = T)
    ) |>
    ungroup() |>
    pivot_wider(id_cols = ExpUnit, names_from = Group, values_from = c(CT_ControlGene, CT_TargetGene)) |>
    mutate(
      Group1 = CT_TargetGene_Group1 - CT_ControlGene_Group1,
      Group2 = CT_TargetGene_Group2 - CT_ControlGene_Group2
    )
  
  group1_mean = mean(summary_data$Group1, na.rm = T)
  summary_data = summary_data |>
    mutate(
      EXP = exp_code, LAB = lab_code, Replicate = ExpUnit,
      Group1_Perc = 100 * Group1 / group1_mean, Group2_Perc = 100 * Group2 / group1_mean
    ) |>
    select(LAB, EXP, Replicate, Group1, Group2, Group1_Perc, Group2_Perc) |>
    
    # Make replicates into a sequential numbering
    filter(!is.na(Replicate)) |>
    group_by(LAB) |>
    mutate(Replicate = Replicate |> as.factor() |> as.integer())
  
  
  if (is.null(suffix))
    export_file(summary_data, exp_code, lab_code, "PCR")
  else
    export_file(summary_data, exp_code, lab_code, "PCR", S = suffix)
}

# Function that summarizes a single replication.
build_summary_table_exp = function (exp_code) {
  print(exp_code)
  exp_data = all_data |> filter(EXP == exp_code)
  grouping_col = meta_data |> filter(EXP == exp_code) |> select(LAB, `Experimental Unit Column`)
  
  # Create new column which has the grouping variable for technical replicates
  exp_data = exp_data |> left_join(grouping_col, by = "LAB")
  exp_data$ExpUnit = NA
  
  for (i in 1:nrow(exp_data)) {
    groupcol = exp_data$`Experimental Unit Column`[i]
    exp_data[i, "ExpUnit"] = exp_data[i, groupcol]
  }
  
  rep_labs = exp_data |> pull(LAB) |> unique()
  
  walk(rep_labs, build_summary_single_lab, exp_data, F)
  
  
  # Build the replication summaries using the experimental units that the labs specified
  exp_data = all_data |> filter(EXP == exp_code, !is.na(`Experimental Unit LAB`))
  
  if (nrow(exp_data) > 0) {
    
    grouping_col = meta_data |> filter(EXP == exp_code) |> select(LAB, `Experimental Unit Alternative`)
    
    # Create new column which has the grouping variable for technical replicates
    exp_data = exp_data |> left_join(grouping_col, by = "LAB") |> filter(!is.na(`Experimental Unit Alternative`))
    if (nrow(exp_data) > 0) {
      exp_data$ExpUnit = NA
      
      for (i in 1:nrow(exp_data)) {
        groupcol = exp_data$`Experimental Unit Alternative`[i]
        exp_data[i, "ExpUnit"] = exp_data[i, groupcol]
      }
      
      rep_labs = exp_data |> pull(LAB) |> unique()
      
      walk(rep_labs, build_summary_single_lab, exp_data, T, "LABUNIT")
    }
  }
}

# Function to read and summarize each file, by experiment, generating the table that is expected by the general analysis script.
summarize_all_tables = function (experiments) {
  walk(experiments, build_summary_table_exp)
}

experiments = all_data$EXP |> unique()

summarize_all_tables(experiments)

write_tsv(file_to_exp_dict, "replication-results/PCR_results_dict.tsv")

rm(list = ls(all = TRUE))
