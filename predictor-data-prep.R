##### EXP level predictor data preparation #####

# Read the data and select the desired predictor columns
PRED_DATA_EXP_LEVEL = read_excel("other-data/predictor-data.xlsx", na = c("","NA", "not applicable", "-"))

SURVEY_DATA = read_csv2("other-data/PRED_DATA_SURVEY.csv")

PRED_DATA_EXP_LEVEL = PRED_DATA_EXP_LEVEL |>
  left_join(SURVEY_DATA, by = "EXP") |>
  mutate(
    `Hedges's g` = hedges_g(`Cohen’s d`, `Assumed Control Sample Size` + `Assumed Treated Sample Size`)
  )

# Compute predictor variables from raw data
PRED_DATA_EXP_LEVEL = PRED_DATA_EXP_LEVEL |>
  rowwise() |>
  mutate(
    `Hedges's g` = abs(`Hedges's g`),
    Landis4_Perc = mean(c(
      `Randomization` |> str_detect("yes"),
      `Blinding` |> str_detect("(yes|automated)"),
      `Sample size calculation` |> str_detect("yes"),
      `Inclusion/exclusion criteria` |> str_detect("yes")
    ), na.rm = T),
    Landis4_Perc = ifelse(Method == "Elevated Plus Maze", Landis4_Perc, NA),
    `Biological model (animal)` = ifelse(`Biological model` %in% c("animal", "primary culture"), T, F),
    EPM = Method == "Elevated Plus Maze",
    MTT = Method == "MTT Assay",
    PCR = Method == "RT-PCR"
  ) |>
  select(
    -c(Method,`Randomization`,`Blinding`,`Sample size calculation`,`Inclusion/exclusion criteria`, `Biological model`)
  )

PRED_DATA_EXP_LEVEL = PRED_DATA_EXP_LEVEL |>
  select(
    `DOI`,
    `EXP`,
    `Biological model (animal)`,
    EPM,
    MTT,
    PCR,
    `Hedges's g`,
    `P-value (calculated)`,
    `Quality of reporting index, from gaps in original protocol`,
    Landis4_Perc,
    `Normalized impact factor`,
    `Year of online publication`,
    `Number of citations (first 2 years)`,
    `Last author’s academic age`,
    `Number of papers 5 years before publishing the article`,
    `Position in the Folha Ranking (current)`,
    `Survey - Replication`,
    `Survey - Effect Size`,
    `Survey - Challenge`
  )

# Rename columns
colnames(PRED_DATA_EXP_LEVEL) = c(
  "DOI",
  "EXP",
  "Animal Model",
  "EPM",
  "MTT",
  "PCR",
  "Hedges's g",
  "p-value",
  "Reporting",
  "Bias Control",
  "Impact Factor",
  "Year",
  "Citations",
  "Academic Age",
  "Publications",
  "Institution Ranking",
  "Survey - Replication",
  "Survey - Effect Size",
  "Survey - Challenge"
)

##### REP level predictor data preparation #####

# Read the data and select the desired predictor columns
PRED_DATA_REP_LEVEL = read_excel("other-data/inclusion_sets.xlsx", na = c("","NA", "not applicable", "-"))

PRED_DATA_REP_LEVEL = PRED_DATA_REP_LEVEL |>
  select(
    `EXP`,
    `LAB`,
    `Quest. 2 - Changes from original`,
    `Validation - Mean score`,
    mean_years_since_first_paper_protocol,
    mean_n_papers_protocol,
    mean_years_since_first_paper_data_collection,
    mean_n_papers_data_collection,
    ruf_all
  ) |> distinct(EXP, LAB, .keep_all = T)

# Rename columns
colnames(PRED_DATA_REP_LEVEL) = c(
  "EXP",
  "LAB",
  "Protocol deviation (Lab)",
  "Protocol deviation (Committee)",
  "Academic age (Protocol Team)",
  "Publications (Protocol Team)",
  "Academic age (Data Collection Team)",
  "Publications (Data Collection Team)",
  "Institution Ranking"
)
