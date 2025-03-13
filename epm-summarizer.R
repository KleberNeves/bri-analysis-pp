p_load(reshape2)

options(dplyr.summarise.inform = FALSE)

source("functions_summarizer.R")

# Read the complete dataset with all EPM measures
all_data = read_csv2("./gathered-data/EPM_data.csv", show_col_types = F)

meta_data = read_csv2("./gathered-data/EPM_meta.csv", show_col_types = F)

experiments = all_data$EXP |> unique()

# the main dataset excludes all measures/experimental units that failed to meet an inclusion criterion or 
# were recommended to be removed by the lab post hoc
data_included = all_data |> filter(Recommend_exclusion == "No")

# if we want to include the values that were not a pre-registered exclusion criterion, un-comment the following line:
#data_included = all_data |> filter(Recommend_exclusion!="Yes, pre-defined criterion")

file_to_exp_dict = tibble(Filename = character(0), LAB = character(0), EXP = character(0))

# Getting all primary outcomes
for (E in experiments) {
  # Filter for each experiment
  data_filter = data_included |> filter(EXP == E)

  # Select only the primary outcome
  primary = (meta_data |> filter(EXP ==E))$`Primary outcome`[1]
  data_filter = data_filter |> select(LAB, Animal_id, Litter_id, Cage_id, Group, all_of(primary))
  data_filter = data_filter |> rename("Outcome" = all_of(primary))
  
  # Make adjustments based on the experimental unit 
  EU = (meta_data |> filter(EXP ==E))$`Grouping Variable Name`[1]
  if (EU == "Animal_id") {
    data_EU = data_filter |> select(-c(Animal_id, Litter_id, Cage_id))
  } 
  if (EU == "Litter_id") {
    data_EU = data_filter |> group_by(LAB, Group, Litter_id) |> summarise(Outcome = mean(Outcome, na.rm=T))
  } 
  if (EU == "Cage_id") {
    data_EU = data_filter |> group_by(LAB, Group, Cage_id) |> summarise(Outcome = mean(Outcome, na.rm=T))
  }
  
  # Clean and organize table for the desired format
  data_EU = data_EU |> select(LAB, Group, Outcome)
  data_EU$Replicate = c(1:nrow(data_EU))
  data_wide = spread(data_EU, key = Group, value = Outcome)
  data_wide = data_wide |> select(c("LAB", "Replicate", "Group1", "Group2"))

  for (L in unique(data_wide$LAB)) {
    print(paste("primary", L, E))
    # Export each table to a folder for all primary outcomes
    data_wide_lab = data_wide |> filter(LAB == L)
    
    group1_mean = mean(data_wide_lab$Group1, na.rm = T)
    data_wide_lab = data_wide_lab |>
      mutate(
        Group1_Perc = 100 * Group1 / group1_mean,
        Group2_Perc = 100 * Group2 / group1_mean
      )
    
    export_file(data_wide_lab, E, L, "EPM/primary_outcomes")
  }
}

# Getting all secondary outcomes
for (E in experiments) {
  # Filter for each experiment
  data_filter = data_included |> filter(EXP == E)

  # Defines the primary outcome, to be excluded
  primary = (meta_data |> filter(EXP ==E))$`Primary outcome`[1]

  # Lists all outcomes for which there is at least one valid value
  all_outcomes = data_filter |>
    select(-c(EXP, LAB, Animal_id, Cage_id, Litter_id, Batch_id, Group, Group_Exp, Recommend_exclusion)) 
  all_outcomes = all_outcomes[, colSums(!is.na(all_outcomes)) > 0]
  # Gets all outcomes and removes the primary
  list.outcomes = names(all_outcomes)
  secondary = list.outcomes[!list.outcomes %in% primary]

  # Loops for each secondary outcome listed above
  for (S in secondary) {
    # Filter for each outcome
    data_S = data_filter |> select(LAB, Animal_id, Litter_id, Cage_id, Group, all_of(S))
    data_S = data_S |> rename("Outcome" = all_of(S))
    
    # Make adjustments based on the experimental unit 
    EU = (meta_data |> filter(EXP ==E))$`Grouping Variable Name`[1]
    if (EU == "Animal_id") {
      data_EU = data_S |> select(-c(Animal_id, Litter_id, Cage_id))
    } 
    if (EU == "Litter_id") {
      data_EU = data_S |> group_by(LAB, Group, Litter_id) |> summarise(Outcome = mean(Outcome, na.rm=T))
    } 
    if (EU == "Cage_id") {
      data_EU = data_S |> group_by(LAB, Group, Cage_id) |> summarise(Outcome = mean(Outcome, na.rm=T))
    }
  
    # Clean and organize table for the desired format
    data_EU = data_EU |> select(LAB, Group, Outcome)
    data_EU$Replicate = c(1:nrow(data_EU))
    data_wide = spread(data_EU, key = Group, value = Outcome) 
    data_wide = data_wide |> select(c("LAB", "Replicate", "Group1", "Group2"))
    
    for (L in unique(data_wide$LAB)) {
      print(paste("secondary", S, L, E))
      # Export each table to a folder for all secondary outcomes
      data_wide_lab = data_wide |> filter(LAB == L)
      
      group1_mean = mean(data_wide_lab$Group1, na.rm = T)
      data_wide_lab = data_wide_lab |>
        mutate(
          Group1_Perc = 100 * Group1 / group1_mean,
          Group2_Perc = 100 * Group2 / group1_mean
        )
      
      export_file(data_wide_lab, E, L, S=S, "EPM/secondary_outcomes")
    }
  }
}

write_tsv(file_to_exp_dict, "./replication-results/EPM_results_dict.tsv")

# PS: the replicate code created does not mean anything, it's just to adjust to the desired format for the main analysis.

rm(list = ls(all = TRUE))
