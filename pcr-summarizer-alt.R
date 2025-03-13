library(tidyverse)
library(readxl)
library(data.table)
library(openxlsx)

options(dplyr.summarise.inform = FALSE)

source("functions_summarizer.R")

# Reads the complete dataset with all PCR measures
all_data = fread("./gathered-data/PCR_data.csv", dec = ",", na.strings = c("","NA", "Undetermined", "undetermined")) |> as_tibble() |> filter(!is.na(Group)) |> filter(is.na(`Exclude?`))

all_data$`CT Gene of Interest` = all_data$`CT Gene of Interest` |> str_replace_all("No amplification", "Inf") |> str_replace_all(",", ".") |> as.numeric()

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
  if (outcome_expression == "?") {
    return (1)
  }
  # Calculate the primary outcome for each method
  if (outcome_expression == "2-DeltaDeltaCT") {
    # 2-DeltaDeltaCT ####
    summary_data = rep_data |>
      group_by(ExpUnit, Group) |>
      summarise(
        CT_ControlGene = mean(c(`CT Control Gene I`, `CT Control Gene II`), na.rm = T),
        CT_TargetGene = mean(`CT Gene of Interest`, na.rm = T)
      ) |>
      ungroup() |>
      mutate(
        deltaCT = CT_TargetGene - CT_ControlGene
      ) |>
      pivot_wider(id_cols = c("ExpUnit"), names_from = Group, values_from = deltaCT) |>
      rename(
        Group1_DeltaCT = Group1,
        Group2_DeltaCT = Group2
      )
    
    if (is_paired) {
      summary_data = summary_data |>
        mutate(
          Group1_DeltaDeltaCT = 0,
          Group2_DeltaDeltaCT = Group2_DeltaCT - Group1_DeltaCT
        )
    } else {
      group1_mean = mean(summary_data$Group1_DeltaCT, na.rm = T)
      summary_data = summary_data |>
        mutate(
          Group1_DeltaDeltaCT = Group1_DeltaCT - group1_mean,
          Group2_DeltaDeltaCT = Group2_DeltaCT - group1_mean
        )
    }
    
    summary_data = summary_data |>
      mutate(
        Group1_2_DeltaDeltaCT = 2 ^ -Group1_DeltaDeltaCT,
        Group2_2_DeltaDeltaCT = 2 ^ -Group2_DeltaDeltaCT
      ) |>
      mutate(
        Group1 = Group1_2_DeltaDeltaCT,
        Group2 = Group2_2_DeltaDeltaCT
      )
    
  
  } else if (outcome_expression == "2-DeltaDeltaCT no amplification") {
    # 2-DeltaDeltaCT no amplification ####    
      summary_data = rep_data |>
        group_by(ExpUnit, Group) |>
        summarise(
          CT_ControlGene = mean(c(`CT Control Gene I`, `CT Control Gene II`), na.rm = T),
          CT_TargetGene = mean(`CT Gene of Interest`, na.rm = T)
        ) |>
        ungroup() |>
        mutate(
          deltaCT = CT_TargetGene - CT_ControlGene
        ) |>
        pivot_wider(id_cols = c("ExpUnit"), names_from = Group, values_from = deltaCT) |>
        rename(
          Group1_DeltaCT = Group1,
          Group2_DeltaCT = Group2
        )
      
      summary_data = summary_data |>
        mutate(
          Group1_2_DeltaCT = 2 ^ -Group1_DeltaCT,
          Group2_2_DeltaCT = 2 ^ -Group2_DeltaCT
        )
      
      group1_mean = mean(summary_data$Group1_2_DeltaCT, na.rm = T)
      summary_data = summary_data |>
        mutate(
          Group1_2_DeltaDeltaCT = Group1_2_DeltaCT / group1_mean,
          Group2_2_DeltaDeltaCT = Group2_2_DeltaCT / group1_mean
        ) |>
        mutate(
          Group1 = Group1_2_DeltaDeltaCT,
          Group2 = Group2_2_DeltaDeltaCT
        )
    } else if (outcome_expression == "2-DeltaCT") {
      # 2-DeltaCT ####
      summary_data = rep_data |>
        group_by(ExpUnit, Group) |>
        summarise(
          CT_ControlGene = mean(c(`CT Control Gene I`, `CT Control Gene II`), na.rm = T),
          CT_TargetGene = mean(`CT Gene of Interest`, na.rm = T)
        ) |>
        ungroup() |>
        pivot_wider(id_cols = ExpUnit, names_from = Group, values_from = c(CT_ControlGene, CT_TargetGene)) |>
        mutate(
          deltaCT_Group1 = CT_TargetGene_Group1 - CT_ControlGene_Group1,
          deltaCT_Group2 = CT_TargetGene_Group2 - CT_ControlGene_Group2,
          Group1 = (2 ^ -deltaCT_Group1),
          Group2 = (2 ^ -deltaCT_Group2)
        )
      
      group1_norm = mean(summary_data$Group1, na.rm = T)
      summary_data = summary_data |>
        mutate(
          Group1 = Group1 / group1_norm,
          Group2 = Group2 / group1_norm
        )
    } else if (outcome_expression == "2-DeltaDeltaCT Single Norm") {
      # 2-DeltaDeltaCT Single Norm ####
      summary_data = rep_data |>
        group_by(ExpUnit, Group) |>
        summarise(
          CT_ControlGene = mean(c(`CT Control Gene I`, `CT Control Gene II`), na.rm = T),
          CT_TargetGene = mean(`CT Gene of Interest`, na.rm = T)
        ) |>
        ungroup() |>
        mutate(
          deltaCT = CT_TargetGene - CT_ControlGene
        )
      
      summary_data = rep_data |>
        pivot_wider(id_cols = c("ExpUnit"), names_from = Group, values_from = deltaCT) |>
        rename(
          Group1_DeltaCT = Group1,
          Group2_DeltaCT = Group2,
          GroupNorm_DeltaCT = GroupNorm
        )
      
      group1_mean = mean(summary_data$GroupNorm, na.rm = T)
      summary_data = summary_data |>
        mutate(
          Group1_DeltaDeltaCT = Group1_DeltaCT - group1_mean,
          Group2_DeltaDeltaCT = Group2_DeltaCT - group1_mean
        )
      
      summary_data = summary_data |>
        mutate(
          Group1_2_DeltaDeltaCT = 2 ^ -Group1_DeltaDeltaCT,
          Group2_2_DeltaDeltaCT = 2 ^ -Group2_DeltaDeltaCT
        ) |>
        mutate(
          Group1 = Group1_2_DeltaDeltaCT,
          Group2 = Group2_2_DeltaDeltaCT
        )
    } else if (outcome_expression == "Optical Density (a.u)") {
      # Optical density ####
      summary_data = rep_data |>
        group_by(ExpUnit, Group) |>
        summarise(
          BandTarget = mean(`Band Density Gene of Interest`, na.rm = T),
          BandControl = mean(`Band Density Control Gene`, na.rm = T)
        ) |>
        ungroup() |>
        mutate(
          RelativeExpression = BandTarget / BandControl
        ) |>
        pivot_wider(id_cols = ExpUnit, names_from = Group, values_from = c(RelativeExpression))
      
      if (is_paired) {
        summary_data = summary_data |>
          mutate(
            Group2 = Group2 / Group1,
            Group1 = 1
          )
      } else {
        group1_norm = mean(summary_data$Group1, na.rm = T)
        summary_data = summary_data |>
          mutate(
            Group2 = Group2 / group1_norm,
            Group1 = Group1 / group1_norm
          )
      }
    } else if (outcome_expression == "Pfaffl") {
      # Pfaffl ####
      summary_data = rep_data |>
        group_by(ExpUnit, Group) |>
        summarise(
          CT_ControlGeneI = mean(`CT Control Gene I`, na.rm = T),
          CT_ControlGeneII = mean(`CT Control Gene II`, na.rm = T),
          CT_TargetGene = mean(`CT Gene of Interest`, na.rm = T),
          Efficiency_ControlI = mean(`Efficiency of primer pair of control I`, na.rm = T),
          Efficiency_ControlII = mean(`Efficiency of primer pair of control II`, na.rm = T),
          Efficiency_Target = mean(`Efficiency of primer pair of interest`, na.rm = T)
        ) |> ungroup()
      
      EF_CI = mean(summary_data$Efficiency_ControlI, na.rm = T)
      EF_CII = mean(summary_data$Efficiency_ControlII, na.rm = T)
      EF_T = mean(summary_data$Efficiency_Target, na.rm = T)
      
      summary_data = summary_data |>
        pivot_wider(id_cols = c("ExpUnit"), names_from = Group, values_from = c(CT_ControlGeneI, CT_ControlGeneII, CT_TargetGene))
      
      group1_meantarget = mean(summary_data$CT_TargetGene_Group1, na.rm = T)
      group1_meancontrol1 = mean(summary_data$CT_ControlGeneI_Group1, na.rm = T)
      group1_meancontrol2 = mean(summary_data$CT_ControlGeneI_Group2, na.rm = T)
      
      summary_data = summary_data |>
        mutate(
          Group1_DeltaCT_Target = CT_TargetGene_Group1 -  group1_meantarget,
          Group1_DeltaCT_ControlI = CT_ControlGeneI_Group1 -  group1_meancontrol1,
          Group1_DeltaCT_ControlII = CT_ControlGeneII_Group1 -  group1_meancontrol2,
          Group2_DeltaCT_Target = CT_TargetGene_Group2 -  group1_meantarget,
          Group2_DeltaCT_ControlI = CT_ControlGeneI_Group2 -  group1_meancontrol1,
          Group2_DeltaCT_ControlII = CT_ControlGeneII_Group2 -  group1_meancontrol2,

          Group1_ExpRelative_ControlI = EF_CI ^ -Group1_DeltaCT_ControlI,
          Group1_ExpRelative_ControlII = EF_CII ^ -Group1_DeltaCT_ControlII,
          Group1_ExpRelative_Target = EF_T ^ -Group1_DeltaCT_Target,          
          Group2_ExpRelative_ControlI = EF_CI ^ -Group2_DeltaCT_ControlI,
          Group2_ExpRelative_ControlII = EF_CII ^ -Group2_DeltaCT_ControlII,
          Group2_ExpRelative_Target = EF_T ^ -Group2_DeltaCT_Target
        )
      
      summary_data = summary_data |>
        mutate(
          Group1_LogExpRelative_ControlI = log(Group1_ExpRelative_ControlI),
          Group1_LogExpRelative_ControlII = ifelse(is.na(Group1_ExpRelative_ControlII), Group1_LogExpRelative_ControlI, log(Group1_ExpRelative_ControlII)),
          Group1_MeanLog = (Group1_LogExpRelative_ControlI + Group1_LogExpRelative_ControlII) / 2,
          Group1_ExpMeanLog = exp(Group1_MeanLog),
          Group1_RelativeExpression = Group1_ExpRelative_Target / Group1_ExpMeanLog,
          Group2_LogExpRelative_ControlI = log(Group2_ExpRelative_ControlI),
          Group2_LogExpRelative_ControlII = ifelse(is.na(Group2_ExpRelative_ControlII), Group2_LogExpRelative_ControlI, log(Group2_ExpRelative_ControlII)),
          Group2_MeanLog = (Group2_LogExpRelative_ControlI + Group2_LogExpRelative_ControlII) / 2,
          Group2_ExpMeanLog = exp(Group2_MeanLog),
          Group2_RelativeExpression = Group2_ExpRelative_Target / Group2_ExpMeanLog
          
        ) |>
        
        mutate(
          Group1 = Group1_RelativeExpression,
          Group2 = Group2_RelativeExpression
        )
      
    }
  
  # write_tsv(summary_data, file = paste0(exp_code," ", lab_code, " INTERMEDIARY.csv"))
  
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
  
  
  exp_code = paste0("ALT", exp_code)
  if (is.null(suffix))
    export_file(summary_data, exp_code, lab_code, "PCR_ALT")
  else
    export_file(summary_data, exp_code, lab_code, "PCR_ALT", S = suffix)
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

write_tsv(file_to_exp_dict, "replication-results/PCR_ALT_results_dict.tsv")

rm(list = ls(all = TRUE))
