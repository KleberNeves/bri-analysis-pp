add_name <-  function(tbl, name) {
  if(grepl("flextable", paste(class(tbl), collapse = " ")) == FALSE) {
    return(
      tbl |>
        as_flex_table() |>
        add_header_lines(values = name, top = TRUE) |>
        hline_top(
          border = fp_border_default(width = 0),
          part = "header"
        ))
  } else{
    tbl |>
      add_header_lines(values = name, top = TRUE) |>
      hline_top(
        border = fp_border_default(width = 0),
        part = "header"
      )
  }
}

save_tbl <- function(
    table_list,
    filename,
    date_hour = FALSE,
    open_folder = FALSE
) {
  
  # Getting the path
  directory <- dirname(filename)
  actual_filename <- basename(filename)
  
  # Check if it exists, otherwise create it
  if (grepl("/", filename)) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }
  
  ## Check if they are lists
  if (grepl("list", paste(class(table_list), collapse = " ")) == FALSE) {
    if (length(table_list) == 7) {
      doc <- read_docx()
      doc <- body_add_flextable(doc, value = table_list)
      return(print(doc, target = filename))
    }
    stop("The table_list parameter must be a list!\nExample: list(tbl1, tbl2, ...).")
  }
  
  
  
  ## Create the document for the tables
  doc <- read_docx()
  
  ## Loop inside the list of lists
  for (i in seq_along(table_list)) {
    table_list_i <- table_list[[i]]
    
    # If it is a list, loop through it
    if (grepl("list", paste(class(table_list_i), collapse = " ")) == TRUE) {
      for (j in seq_along(table_list_i)) {
        flex_table <- table_list_i[[j]]
        
        ## Convert objects to flextable
        if (grepl("gtsummary", paste(class(flex_table), collapse = " ")) == TRUE) {
          flex_table <- as_flex_table(flex_table)
          cat("Table from the list", i, "at position", j, "has been converted to a flextable.")
        } else if (grepl("data.frame", paste(class(table_list_i), collapse = " ")) == TRUE) {
          flex_table <- flextable(flex_table)
          cat("Table from the list", i, "at position", j, "has been converted to a flextable.")
        }
        
        if (i == 1 && j == 1) {
          # first table
          doc <- body_add_flextable(doc, value = flex_table) |>
            body_add_par(run_linebreak()) |>
            body_add_par(run_linebreak()) |>
            body_add_par(run_linebreak())
        } else if (j == length(table_list_i)) {
          # last table
          if (i == length(table_list)) {
            doc <- body_add_flextable(doc, value = flex_table)
          } else {
            doc <- body_add_flextable(doc, value = flex_table) |> body_add_break(pos = "after")
          }
        } else {
          # subsequent tables
          doc <- body_add_flextable(doc, value = flex_table) |>
            body_add_par(run_linebreak()) |>
            body_add_par(run_linebreak()) |>
            body_add_par(run_linebreak())
        }
      }
    }
    
    ## If it's not a list
    ## Input is: list(tbl1, tbl2...)
    else {
      ## Convert to flextable
      if (grepl("gtsummary", paste(class(table_list_i), collapse = " ")) == TRUE) {
        table_list_i <- as_flex_table(table_list_i)
        cat("Table at position", i, "in the list has been converted to a flextable.")
      } else if (grepl("data.frame", paste(class(table_list_i), collapse = " ")) == TRUE) {
        table_list_i <- flextable(table_list_i)
        cat("Table at position", i, "in the list has been converted to a flextable.")
      }
      
      if (i == length(table_list)) {
        doc <- body_add_flextable(doc, value = table_list_i)
      } else {
        doc <- body_add_flextable(doc, value = table_list_i) |> body_add_break(pos = "after")
      }
    }
  }
  
  # add timestamp
  if(date_hour == TRUE){
    filename <- str_glue(
      "{directory}/",
      format(Sys.Date(), "%Y%m%d"),
      "_",
      format(Sys.time(), "%H%M"),
      "_",
      actual_filename
    )
  }
  
  print(doc, target = filename)
  
  # go to folder
  if(open_folder == TRUE){
    browseURL(str_glue(getwd(), "/{directory}"))
  }
}
