p_load(tidyverse, readxl, stringr, readxl, data.table, openxlsx)

options(dplyr.summarise.inform = FALSE)

source("functions_summarizer.R")

# Reads the complete dataset with all MTT measures
all_data = fread("./gathered-data/MTT_data.csv", dec = ",", na.strings = c("","NA")) |> as_tibble() |> filter(!is.na(Group)) |> filter(is.na(`Exclude?`))

meta_data = read_csv2("./gathered-data/MTT_meta.csv", na = c("","NA"), locale = locale(decimal_mark = ","))

file_to_exp_dict = tibble(Filename = character(0), LAB = character(0), EXP = character(0))


build_summary_single_lab = function(lab_code, exp_data, suffix = NULL) {
  exp_code = exp_data$EXP |> unique()
  rep_data = exp_data |> filter(LAB == lab_code)
  
  # Get pairing info
  is_paired = (meta_data |> filter(EXP == exp_code, LAB == lab_code) |> pull(`Paired Summary`)) == "yes"
  
  # Check if the blanks are per plate (but with multiple experimental units in the same plate)
  is_per_plate_blank = lab_code %in% (rep_data |> filter(Group == "Group1_blank_plate" | Group== "Group2_blank_plate") |> pull(LAB) |> unique())
  
  summary_data = rep_data
  if (!is_per_plate_blank) {
    # For experiments where blanks are not per plate, make single plate ID
    summary_data = summary_data |>
      mutate(`Plate ID` = "IGNORE")
  }
  
  # Adjust data and average over the technical replicates
  summary_data = summary_data |>
    select(LAB, ExpUnit, Group, `Plate ID`, `Optical Density (a.u.)`) |>
    filter(!is.na(Group)) |>
    # "Tare"/calibrate the measurements, so that the minimum OD value is 0, in case there are negative measurements of OD
    # mutate(`Optical Density (a.u.)` = `Optical Density (a.u.)` - min(`Optical Density (a.u.)`)) |>
    mutate(`Optical Density (a.u.)` = ifelse(`Optical Density (a.u.)` < 0, 0, `Optical Density (a.u.)`)) |>
    group_by(LAB, ExpUnit, Group, `Plate ID`) |>
    summarise(
      mean_OD = mean(`Optical Density (a.u.)`, na.rm = T), .groups = "keep"
    ) |>
    ungroup()
  
  # Add Plate level summaries of blanks when needed
  if (is_per_plate_blank) {
    per_plate_blanks = rep_data |> 
      group_by(LAB, `Plate ID`, Group) |>
      summarise(
        mean_OD = mean(`Optical Density (a.u.)`, na.rm = T), .groups = "keep"
      ) |>
      filter(stringr::str_detect(Group, "_plate")) |>
      ungroup() |>
      select(`Plate ID`, Group, mean_OD)
    
    summary_data = rbind(
      summary_data |> filter(Group == "Group1") |>
        left_join(per_plate_blanks |> filter(Group == "Group1_blank_plate"), by = "Plate ID"),
      summary_data |> filter(Group == "Group2") |>
        left_join(per_plate_blanks |> filter(Group == "Group2_blank_plate"), by = "Plate ID")
    ) |>
      # Subtract blanks per plate
      mutate(
        mean_OD = ifelse(is.na(mean_OD.y), mean_OD.x, mean_OD.x - mean_OD.y)
      ) |>
      select(1,2,3,4,8) |>
      group_by(LAB, ExpUnit, Group.x) |>
      summarise(
        mean_OD = mean(mean_OD, na.rm = T), .groups = "keep"
      )
  } else {
    summary_data = summary_data |>
      select(-`Plate ID`)  
  }
  
  summary_data = summary_data |>
    filter(!is.na(ExpUnit)) |>
    `colnames<-`(c("LAB", "OrigReplicate", "Group", "Value")) |>
    # Make replicates into a sequential numbering
    group_by(LAB) |>
    mutate(Replicate = OrigReplicate |> as.factor() |> as.integer()) |>
    pivot_wider(
      id_cols = c(LAB, Replicate), values_from = Value, names_from = Group
    )
  
  # If you don't have blanks in the data at this point, it means it was a per plate blank and it has been processed above
  if (!is_per_plate_blank) {
    
    if (!("Blank_all_groups" %in% colnames(summary_data))) {
      summary_data$Blank_all_groups = NA
    }
    if (!("Group1_blank" %in% colnames(summary_data))) {
      summary_data$Group1_blank = NA
    }
    if (!("Group2_blank" %in% colnames(summary_data))) {
      summary_data$Group2_blank = NA
    }
    if (!("Group1_blank_plate" %in% colnames(summary_data))) {
      summary_data$Group1_blank_plate = NA
    }
    if (!("Group2_blank_plate" %in% colnames(summary_data))) {
      summary_data$Group2_blank_plate = NA
    }
    
    summary_data = summary_data |>
      mutate(
        Group1 = Group1 - ifelse(
          !is.na(Group1_blank),
          Group1_blank, ifelse(
            !is.na(Blank_all_groups),
            Blank_all_groups, 0
          )
        ),
        Group2 = Group2 - ifelse(
          !is.na(Group2_blank),
          Group2_blank, ifelse(
            !is.na(Blank_all_groups),
            Blank_all_groups, 0
          )
        )
      )
  }
  
  # If measurements are negative after subtracting blanks, make them 0
  summary_data = summary_data |>
    mutate(
      Group1 = ifelse(Group1 < 0, 0, Group1),
      Group2 = ifelse(Group2 < 0, 0, Group2)
    )
  
  exp_code = rep_data$EXP[1]
  lab_code = rep_data$LAB[1]
  
  summary_data = summary_data |>
    mutate(EXP = exp_code) |>
    select(LAB, EXP, Replicate, Group1, Group2) |>
    filter(!is.na(Group1) | !is.na(Group2))
  
  # Express results as percentages, depending on pairing
  if (is_paired) {
    summary_data = summary_data |>
      mutate(
        Group1_Perc = 100 * Group1 / Group1,
        Group2_Perc = 100 * Group2 / Group1
      )
  } else {
    group1_mean = mean(summary_data$Group1, na.rm = T)
    summary_data = summary_data |>
      mutate(
        Group1_Perc = 100 * Group1 / group1_mean,
        Group2_Perc = 100 * Group2 / group1_mean
      )  
  }
  
  if (is.null(suffix))
    export_file(summary_data, exp_code, lab_code, "MTT")
  else
    export_file(summary_data, exp_code, lab_code, "MTT", S = suffix)
}

# Function that summarizes a single replication.
build_summary_table_exp = function (exp_code) {
  print(exp_code)
  exp_data = all_data |> filter(EXP == exp_code)
  grouping_col = meta_data |> filter(EXP == exp_code) |> select(LAB, `Grouping Variable Name`)
  
  # Create new column which has the grouping variable for technical replicates
  exp_data = exp_data |> left_join(grouping_col, by = "LAB")
  exp_data$ExpUnit = NA
  
  for (i in 1:nrow(exp_data)) {
    groupcol = exp_data$`Grouping Variable Name`[i]
    exp_data[i, "ExpUnit"] = exp_data[i, groupcol]
  }
  
  rep_labs = exp_data |> pull(LAB) |> unique()
  
  walk(rep_labs, build_summary_single_lab, exp_data)
  
  
  # Build the replication summaries using the experimental units that the labs specified
  exp_data = all_data |> filter(EXP == exp_code, !is.na(`ID LAB`))
  
  if (nrow(exp_data) > 0) {
    grouping_col = meta_data |> filter(EXP == exp_code) |> select(LAB, `Grouping Variable Alternative`)
    
    # Create new column which has the grouping variable for technical replicates
    exp_data = exp_data |> left_join(grouping_col, by = "LAB")
    exp_data$ExpUnit = NA
    
    for (i in 1:nrow(exp_data)) {
      groupcol = exp_data$`Grouping Variable Alternative`[i]
      exp_data[i, "ExpUnit"] = exp_data[i, groupcol]
    }
    
    rep_labs = exp_data |> pull(LAB) |> unique()
    
    walk(rep_labs, build_summary_single_lab, exp_data, "LABUNIT")
  }
}

# Function to read and summarize each file, by experiment, generating the table that is expected by the general analysis script.
summarize_all_tables = function (experiments) {
  walk(experiments, build_summary_table_exp)
}

experiments = all_data$EXP |> unique()
summarize_all_tables(experiments)

write_tsv(file_to_exp_dict, "replication-results/MTT_results_dict.tsv")

rm(list = ls(all = TRUE))
