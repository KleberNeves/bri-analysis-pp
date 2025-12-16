source("analysis.R")

inclusion_sets = read_excel("./other-data/inclusion_sets.xlsx", 1)

setcol = "analysis_primary"

inclusion_sets$include_col = inclusion_sets[[setcol]]
exp_list = inclusion_sets |> filter(!is.na(include_col)) |> pull(EXP) |> unique()
rep_list = inclusion_sets |> filter(!is.na(include_col)) |> rowwise() |> mutate(EXPLAB = paste0(EXP, " ", LAB)) |> pull(EXPLAB)

n_sims = 1000
i_sims = 0

run_all_sims = function (dist) {
  sim_exp_sss <<- matrix(nrow = length(exp_list), ncol = n_sims)
  rownames(sim_exp_sss) <<- exp_list
  colnames(sim_exp_sss) <<- paste0("SIM_", 1:n_sims)
  sim_exp_sss <<- as_tibble(sim_exp_sss, rownames = "EXP")
  
  sim_rep_sss <<- matrix(nrow = length(rep_list), ncol = n_sims)
  rownames(sim_rep_sss) <<- rep_list
  colnames(sim_rep_sss) <<- paste0("SIM_", 1:n_sims)
  sim_rep_sss <<- as_tibble(sim_rep_sss, rownames = "EXP")
  
  
  finish_times = tibble(setcol = "start", time = lubridate::now())
  
  result = tibble()
  
  i_sims <<- 1
  while (i_sims <= n_sims) {
    rm(result)
    result = run_all_meta_analyses(
      inclusion_set_column = setcol,
      save_results_to = NULL,
      params = list(
        exclude_outliers = F,
        ma_dist = dist
      ),
      simulated = T
    )
    
    if (all(is.na(result))) {
      print("Error: running again ...")
      next
    }
    
    by_exp_result = result$by_exp |>
      mutate(
        signif = ifelse(
          is.na(fema_pvalue),
          ifelse(
            is.na(pvalue),
            NA,
            pvalue < 0.05
          ),
          fema_pvalue < 0.05
        )
      )
    
    by_rep_result = result$by_rep |>
      mutate(
        signif = ifelse(
          is.na(pvalue),
          NA,
          pvalue < 0.05
        )
      )
    
    if (nrow(sim_exp_sss[,i_sims+1]) != length(by_exp_result$signif) | nrow(sim_rep_sss[,i_sims+1]) != length(by_rep_result$signif)) {
      browser()
      print("Error: running again ...")
      next
    }
    
    sim_exp_sss[,i_sims+1] <<- by_exp_result$signif
    sim_rep_sss[,i_sims+1] <<- by_rep_result$signif
    
    power_check_all_data <<- rbind(power_check_all_data, by_exp_result)
    
    finish_times <<- rbind(finish_times, tibble(setcol = setcol, time = lubridate::now()))
    
    i_sims <<- i_sims + 1
  }
  
  sim_results_exp = sim_exp_sss |>
    rowwise() |>
    mutate(
      N_SIGNIF = sum(c_across(where(is.logical))),
      N_NONSIGNIF = length(c_across(where(is.logical))) - N_SIGNIF,
      PERC_SIGNIF = mean(c_across(where(is.logical)))
    )
  
  sim_results_rep = sim_rep_sss |>
    rowwise() |>
    mutate(
      N_SIGNIF = sum(c_across(where(is.logical))),
      N_NONSIGNIF = length(c_across(where(is.logical))) - N_SIGNIF,
      PERC_SIGNIF = mean(c_across(where(is.logical)))
    )
  
  write_tsv(sim_results_exp, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Results by EXP - MA dist ", dist, ".tsv"))
  write_tsv(sim_results_rep, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Results by REP - MA dist ", dist, ".tsv"))
}

dir.create("./post-hoc-power-results")

power_sim_check = tibble()
power_check_all_data = tibble()

run_all_sims("z")
write_tsv(power_sim_check, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Data Generation Check ", "z", ".tsv"))
write_tsv(power_check_all_data, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Data Generation Full Check ", "z", ".tsv"))

power_sim_check = tibble()
power_check_all_data = tibble()

run_all_sims("t")
write_tsv(power_sim_check, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Data Generation Check ", "t", ".tsv"))
write_tsv(power_check_all_data, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Data Generation Full Check ", "t", ".tsv"))

power_sim_check = tibble()
power_check_all_data = tibble()

run_all_sims("knha")
write_tsv(power_sim_check, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Data Generation Check ", "knha", ".tsv"))
write_tsv(power_check_all_data, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Data Generation Full Check ", "knha", ".tsv"))

power_sim_check = tibble()
power_check_all_data = tibble()

run_all_sims("bigexp")
write_tsv(power_sim_check, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Data Generation Check ", "bigexp", ".tsv"))
write_tsv(power_check_all_data, paste0("./post-hoc-power-results/Post Hoc Power Simulations - Data Generation Full Check ", "bigexp", ".tsv"))

