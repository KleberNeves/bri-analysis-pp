# Loads all required libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_boot(load = TRUE)
pacman::p_load(
  "bit64",
  "evaluate",
  "likert",
  "tidyverse",
  "metafor",
  "readxl",
  "scales",
  "irr",
  "esc",
  "data.table",
  "stringi",
  "ggtext",
  "cowplot",
  "RColorBrewer",
  "flextable",
  "ggnewscale",
  "flowchart",
  "forcats",
  "kableExtra",
  "here",
  "viridisLite",
  "gtsummary",
  "janitor",
  "sjlabelled",
  "openxlsx",
  "officer",
  "Hmisc",
  "ggthemes",
  "ggpubr",
  "plotly",
  "pheatmap",
  "lubridate"
)


dir.create("./output")

# Global definitions
inclusion_sets = read_excel("./other-data/inclusion_sets.xlsx", 1)

# Order for experiments in figures
exp_general_order = inclusion_sets |> select(EXP) |> distinct() |>
  mutate(
    method = str_extract(EXP, "(MTT|PCR|EPM)"),
    n = str_extract(EXP, "[0-9]+") |> as.integer(),
    n_text = ifelse(n < 10, str_c("0", n, sep = ""), as.character(n)),
    EXP_ORDER = str_c(method, n_text)
  ) |>
  arrange(EXP_ORDER) |>
  pull(EXP) |>
  rev()

rm(inclusion_sets)

# Loads auxiliary scripts with the actual functions

message("Loading reproducibility functions ...")
source("reproducibility.R")

message("Loading plotting functions ...")
source("plots.R")

message("Loading reproducibility rate functions ...")
source("aggregate.R")

message("Loading predictor analysis functions ...")
source("predictor-analysis.R")

message("Loading table functions ...")
source("table-functions.R")

message("Done!")
