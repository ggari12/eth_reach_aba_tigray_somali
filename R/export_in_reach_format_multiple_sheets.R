export_in_reach_format_multiple_sheets<-function(file_R_1,  sheet_name_1 = "sheet_1",
                                                 file_R_2 = NULL,sheet_name_2= "2",
                                                 file_R_3= NULL,  sheet_name_3= "3",
                                                 file_R_4= NULL,sheet_name_4= "4",
                                                 file_R_5= NULL,  sheet_name_5= "5",
                                                 file_R_6= NULL,sheet_name_6= "6",
                                                 file_R_7= NULL,sheet_name_7= "7",
                                                 file_R_8= NULL,sheet_name_8= "8",
                                                 file_R_9= NULL,sheet_name_9= "9",
                                                 file_R_10= NULL,sheet_name_10= "10",
                                                 file_R_11= NULL,sheet_name_11= "11",
                                                 file_R_12= NULL,sheet_name_12= "12",
                                                 export_file_name,
                                                 export_directory = ""){
  
#set dates back to characther so Excel shows them correctly
  if(length(names(Filter(is.POSIXct,file_R_1 )))>0){
    file_R_1<-file_R_1 %>% mutate_if(is.POSIXct,as.character)
  }
  
  # Create workbook
  wb <- createWorkbook()
  #for file_R_1
  # Add a worksheets
  addWorksheet(wb, sheet = sheet_name_1, gridLines = T)
  # write data
  writeData(wb, sheet = sheet_name_1, file_R_1, rowNames = F)
  # set column widths
  setColWidths(wb, sheet = sheet_name_1, cols = 1:ncol(file_R_1), widths = rep(15,ncol(file_R_1)))
  setRowHeights(wb, sheet = sheet_name_1,rows = 1, heights = 17)
  
  # header style
  header_style <- 
    createStyle(fontName = "Arial Narrow", fontSize = 11, fontColour = "white",  border = "TopBottomLeftRight ", borderColour = "black", halign = "center", fgFill = "#E34443", textDecoration = "bold")
  addStyle(wb, sheet = sheet_name_1, header_style, rows = 1, cols = 1:ncol(file_R_1), gridExpand = TRUE)
  
  # table body style
  table_body_style1 <-
    createStyle(fontName = "Arial Narrow",fontSize = 7, border = "TopBottomLeftRight ", borderColour = "grey26", borderStyle = "thin", wrapText = F)
  addStyle(wb, sheet = sheet_name_1, table_body_style1, rows = 2:(nrow(file_R_1)+1), cols = 1:ncol(file_R_1), gridExpand = TRUE)

  
  if(!is.null(file_R_2)){
    
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_2 )))>0){
      file_R_2<-file_R_2 %>% mutate_if(is.POSIXct,as.character)
    }
    
  # Add a worksheets
  addWorksheet(wb, sheet = sheet_name_2, gridLines = T)
  # write data
  writeData(wb, sheet = sheet_name_2, file_R_2, rowNames = F)
  # set column widths
  setColWidths(wb, sheet = sheet_name_2, cols = 1:ncol(file_R_2), widths = rep(15,ncol(file_R_2)))
  setRowHeights(wb, sheet = sheet_name_2,rows = 1, heights = 17)
  
  # header style
   addStyle(wb, sheet = sheet_name_2, header_style, rows = 1, cols = 1:ncol(file_R_2), gridExpand = TRUE)

  # table body style
    addStyle(wb, sheet = sheet_name_2, table_body_style1, rows = 2:(nrow(file_R_2)+1), cols = 1:ncol(file_R_2), gridExpand = TRUE)
  }

  if(!is.null(file_R_3)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_3 )))>0){
      file_R_3<-file_R_3 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_3, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_3, file_R_3, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_3, cols = 1:ncol(file_R_3), widths = rep(15,ncol(file_R_3)))
    setRowHeights(wb, sheet = sheet_name_3,rows = 1, heights = 17)
    
    # header style
        addStyle(wb, sheet = sheet_name_3, header_style, rows = 1, cols = 1:ncol(file_R_3), gridExpand = TRUE)

    # table body style
     addStyle(wb, sheet = sheet_name_3, table_body_style1, rows = 2:(nrow(file_R_3)+1), cols = 1:ncol(file_R_3), gridExpand = TRUE)
  }

  if(!is.null(file_R_4)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_4 )))>0){
      file_R_4<-file_R_4 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_4, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_4, file_R_4, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_4, cols = 1:ncol(file_R_4), widths = rep(15,ncol(file_R_4)))
    setRowHeights(wb, sheet = sheet_name_4,rows = 1, heights = 17)
    
    # header style
       addStyle(wb, sheet = sheet_name_4, header_style, rows = 1, cols = 1:ncol(file_R_4), gridExpand = TRUE)

    # table body style
      addStyle(wb, sheet = sheet_name_4, table_body_style1, rows = 2:(nrow(file_R_4)+1), cols = 1:ncol(file_R_4), gridExpand = TRUE)
  }

  if(!is.null(file_R_5)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_5 )))>0){
      file_R_5<-file_R_5 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_5, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_5, file_R_5, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_5, cols = 1:ncol(file_R_5), widths = rep(15,ncol(file_R_5)))
    setRowHeights(wb, sheet = sheet_name_5,rows = 1, heights = 17)
    
    # header style
     addStyle(wb, sheet = sheet_name_5, header_style, rows = 1, cols = 1:ncol(file_R_5), gridExpand = TRUE)

    # table body style
     addStyle(wb, sheet = sheet_name_5, table_body_style1, rows = 2:(nrow(file_R_5)+1), cols = 1:ncol(file_R_5), gridExpand = TRUE)
  }

  if(!is.null(file_R_6)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_6 )))>0){
      file_R_6<-file_R_6 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_6, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_6, file_R_6, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_6, cols = 1:ncol(file_R_6), widths = rep(15,ncol(file_R_6)))
    setRowHeights(wb, sheet = sheet_name_6,rows = 1, heights = 17)
    
    # header style
     addStyle(wb, sheet = sheet_name_6, header_style, rows = 1, cols = 1:ncol(file_R_6), gridExpand = TRUE)

    # table body style
      addStyle(wb, sheet = sheet_name_6, table_body_style1, rows = 2:(nrow(file_R_6)+1), cols = 1:ncol(file_R_6), gridExpand = TRUE)
  }

  if(!is.null(file_R_7)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_7 )))>0){
      file_R_7<-file_R_7 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_7, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_7, file_R_7, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_7, cols = 1:ncol(file_R_7), widths = rep(15,ncol(file_R_7)))
    setRowHeights(wb, sheet = sheet_name_7,rows = 1, heights = 17)
    
    # header style
     addStyle(wb, sheet = sheet_name_7, header_style, rows = 1, cols = 1:ncol(file_R_7), gridExpand = TRUE)

    # table body style
      addStyle(wb, sheet = sheet_name_7, table_body_style1, rows = 2:(nrow(file_R_7)+1), cols = 1:ncol(file_R_7), gridExpand = TRUE)
  }

  if(!is.null(file_R_8)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_8 )))>0){
      file_R_8<-file_R_8 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_8, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_8, file_R_8, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_8, cols = 1:ncol(file_R_8), widths = rep(15,ncol(file_R_8)))
    setRowHeights(wb, sheet = sheet_name_8,rows = 1, heights = 17)
    
    # header style
       addStyle(wb, sheet = sheet_name_8, header_style, rows = 1, cols = 1:ncol(file_R_8), gridExpand = TRUE)

    # table body style
    addStyle(wb, sheet = sheet_name_8, table_body_style1, rows = 2:(nrow(file_R_8)+1), cols = 1:ncol(file_R_8), gridExpand = TRUE)
  }

  if(!is.null(file_R_9)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_9 )))>0){
      file_R_9<-file_R_9 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_9, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_9, file_R_9, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_9, cols = 1:ncol(file_R_9), widths = rep(15,ncol(file_R_9)))
    setRowHeights(wb, sheet = sheet_name_9,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_9, header_style, rows = 1, cols = 1:ncol(file_R_9), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_9, table_body_style1, rows = 2:(nrow(file_R_9)+1), cols = 1:ncol(file_R_9), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_10)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_10 )))>0){
      file_R_10<-file_R_10 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_10, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_10, file_R_10, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_10, cols = 1:ncol(file_R_10), widths = rep(15,ncol(file_R_10)))
    setRowHeights(wb, sheet = sheet_name_10,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_10, header_style, rows = 1, cols = 1:ncol(file_R_10), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_10, table_body_style1, rows = 2:(nrow(file_R_10)+1), cols = 1:ncol(file_R_10), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_11)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_11 )))>0){
      file_R_11<-file_R_11 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_11, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_11, file_R_11, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_11, cols = 1:ncol(file_R_11), widths = rep(15,ncol(file_R_11)))
    setRowHeights(wb, sheet = sheet_name_11,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_11, header_style, rows = 1, cols = 1:ncol(file_R_11), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_11, table_body_style1, rows = 2:(nrow(file_R_11)+1), cols = 1:ncol(file_R_11), gridExpand = TRUE)
  }

  if(!is.null(file_R_12)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_12 )))>0){
      file_R_12<-file_R_12 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_12, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_12, file_R_12, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_12, cols = 1:ncol(file_R_12), widths = rep(15,ncol(file_R_12)))
    setRowHeights(wb, sheet = sheet_name_12,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_12, header_style, rows = 1, cols = 1:ncol(file_R_12), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_12, table_body_style1, rows = 2:(nrow(file_R_12)+1), cols = 1:ncol(file_R_12), gridExpand = TRUE)
  }
  
  saveWorkbook(wb, paste0(export_directory,export_file_name,".xlsx"), overwrite = TRUE)
}


