 #hello sunshine

library(shiny)
 library(dplyr)
                
                  ui <- fluidPage(
                  titlePanel("Reformat DAG or TAG LipidView txt files to use in ShinyLipidCountr"),
                  
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("mode", "Choose Lipid Type:", choices = c("DAG", "TAG")),
                      helpText("Upload the LipidView-generated .txt file here. For TAG, the app will filter the IS for 'IS D5-TAG 17:0/17:1/17:0 (17:1)(-FA 17:1 (NH4))' and 'IS D5-TAG 17:0/17:1/17:0 (17:0)(-FA 17:0 (NH4))' and drop the others. The two IS D5-TAG 17:0 will be sumed up and used as reference. For DAG, the two IS 'IS DAG 15:0/D7-18:1 (15:0)(FA 15:0)' and 'IS DAG 15:0/D7-18:1 (15:0)(FA (d7)18:1 DAG)' will be sumed up and be used as reference.  "),
                      fileInput("file", "Upload LipidView txt File", accept = ".txt"),
                      helpText("On the right you can see the processed output file ready to be used in ShinyLipidCountr. Please check carefully if the file is plausible to you and maybe compare with your manual analysis."),
                      downloadButton("download", "Download Processed File to use in ShinyLipidCountr")
                    ),
                    
                    mainPanel(
                      tableOutput("preview")
                    )
                  )
                )
                
                server <- function(input, output) {
                  processed_data <- reactive({
                    req(input$file)
                    
                    data <- read.table(input$file$datapath, sep = "\t", header = TRUE)
                    
                    groupcol <- data[1, , drop = FALSE]
                    data <- data[-1, , drop = FALSE]
                    
                    if (input$mode == "DAG") {
                      df1 <- data[, 1, drop = FALSE]
                      df2 <- data[, -1, drop = FALSE]
                      df2[] <- lapply(df2, as.numeric)
                      
                      df1[1, 1] <- "IS DG 33:1-d7"
                      df1 <- df1[-2, , drop = FALSE]
                      
                      sum_values <- colSums(df2[1:2, ], na.rm = TRUE)
                      df2[1, ] <- sum_values
                      df2 <- df2[-2, , drop = FALSE]
                      
                      data_m <- cbind(df1, df2)
                      
                      # CLEAN Sample.Name for DAG
                      data_m$Sample.Name <- data_m$Sample.Name |>
                        sub("\\+.*", "", x = _) |>
                        sub("\\(-FA.*?\\)", "", x = _) |>
                        trimws()
                      
                      data_m_sum <- data_m %>%
                        group_by(Sample.Name) %>%
                        summarise(across(everything(), ~ sum(as.numeric(.), na.rm = TRUE))) %>%
                        arrange(desc(Sample.Name == "IS DG 33:1-d7"))
                      
                      data_m_sum[[1]] <- paste0(data_m_sum[[1]], "(DG)")
                      data_m_sum <- rbind(groupcol, data_m_sum)
                      colnames(data_m_sum)[colnames(data_m_sum) == "Sample.Name"] <- "Sample Name"
                      
                    } else if (input$mode == "TAG") {
                      data_filtered <- data[grepl("IS D5-TAG 17:0", data$`Sample.Name`) | !grepl("^IS", data$`Sample.Name`), ]
                      
                      df1 <- data_filtered[, 1, drop = FALSE]
                      df2 <- data_filtered[, -1, drop = FALSE]
                      df2[] <- lapply(df2, as.numeric)
                      
                      df1[1, 1] <- "IS d5-TG 51:1"
                      df1 <- df1[-2, , drop = FALSE]
                      
                      sum_values <- colSums(df2[1:2, ], na.rm = TRUE)
                      df2[1, ] <- sum_values
                      df2 <- df2[-2, , drop = FALSE]
                      
                      data_m <- cbind(df1, df2)
                      
                      # CLEAN Sample.Name for TAG
                      data_m$Sample.Name <- data_m$Sample.Name |>
                        sub("\\+.*", "", x = _) |>
                        sub("\\(-FA.*?\\)", "", x = _) |>
                        trimws()
                      
                      # Rename TAG to TG
                      data_m$Sample.Name <- gsub("TAG", "TG", data_m$Sample.Name)
                      
                      data_m_sum <- data_m %>%
                        group_by(Sample.Name) %>%
                        summarise(across(everything(), ~ sum(as.numeric(.), na.rm = TRUE))) %>%
                        arrange(desc(Sample.Name == "IS d5-TG 51:1"))
                      
                      data_m_sum[[1]] <- paste0(data_m_sum[[1]], "(TG)")
                      data_m_sum <- rbind(groupcol, data_m_sum)
                      colnames(data_m_sum)[colnames(data_m_sum) == "Sample.Name"] <- "Sample Name"
                    }
                    
                    return(data_m_sum)
                  })
                  
                  output$preview <- renderTable({
                    head(processed_data(), 10)
                  })
                  
                  output$download <- downloadHandler(
                    filename = function() {
                      paste0("Processed_", input$mode, "_data.txt")
                    },
                    content = function(file) {
                      write.table(processed_data(), file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
                    }
                  )
                }
                
                shinyApp(ui, server)
