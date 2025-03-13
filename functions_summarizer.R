export_file = function (f, exp_code, lab_code, folder, S=NULL) {
  fn = paste0("replication-results/", folder,
              "/Result - ", lab_code, " ", exp_code, "", S, ".tsv")
  # browser()
  write_tsv(file = fn, f, quote = "all")
  Sys.sleep(0.2)
  
  if (is.null(S)) {
    file_to_exp_dict <<- rbind(
      file_to_exp_dict,
      tibble(Filename = fn, LAB = lab_code, EXP = exp_code, UNIT = "")
    )
  } else {
    file_to_exp_dict <<- rbind(
      file_to_exp_dict,
      tibble(Filename = fn, LAB = lab_code, EXP = exp_code, UNIT = S)
    )
  }
  
  fn = paste0("replication-results/For labs/", folder,
              "/Result - ", lab_code, " ", exp_code, "", S, ".xlsx")
  
  f$Group1 = as.character(f$Group1)
  f$Group2 = as.character(f$Group2)
  f$Group1_Perc = as.character(f$Group1_Perc)
  f$Group2_Perc = as.character(f$Group2_Perc)
  
  f$Group1[is.na(f$Group1)] = ""
  f$Group1_Perc[is.na(f$Group1_Perc)] = ""
  f$Group2[is.na(f$Group2)] = ""
  f$Group2_Perc[is.na(f$Group2_Perc)] = ""
  
  wb = createWorkbook()
  addWorksheet(wb, "Data")
  sht = 1
  writeData(wb, sht, x = f)
  
  setColWidths(wb, sht, cols = 1:7, widths = c(7,7,9,12,12,12,12))
  setRowHeights(wb, sht, rows = 1:(nrow(f)+1), heights = 24)
  
  sty = createStyle(valign = "center", halign = "center", wrapText = T)
  addStyle(wb, sheet = sht, sty, rows = 2:(nrow(f)+1), cols = 1:7, gridExpand = TRUE)
  
  sty = createStyle(valign = "center", halign = "center", wrapText = T, textDecoration = "bold")
  addStyle(wb, sheet = sht, sty, rows = 1, cols = 1:7, gridExpand = TRUE)
  
  saveWorkbook(wb, fn, overwrite = TRUE)
}