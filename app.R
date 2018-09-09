#load libraries ----------------
library(plyr)
library(tidyverse)
library(readxl)
library(writexl)
library(shiny)

source("tab_cbr.R")

# Define UI --------------
ui <- fluidPage(
  
  # Application title
  titlePanel("CBR Indicators Tables"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("df", "Upload an XLSX file",
                multiple = FALSE,
                accept = c(".xlsx"),
                buttonLabel = "Browse...",
                placeholder = "Please upload an XLSX file")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("statusUpload"),
      textOutput("statusTables"),
      uiOutput("ui.action")
      
    )
  )
)

# Define server logic ---------
server <- function(input, output) {
  
  #reactive object of uploaded data, with changes made to relevant columns --------
  df_uploaded <- reactive({
    input_file <- input$df
    
    if (is.null(input_file)) return(NULL)
    else {
      file.rename(input_file$datapath, paste0(input_file$datapath, ".xlsx"))
      
      df_uploading <- read_excel(paste0(input_file$datapath, ".xlsx"))
      
      ## FIX - add check for correct column names here
      
      df_uploading <- df_uploading %>%  
        mutate_all(funs(plyr::mapvalues(., from = c(777,888,999), to = c(NA,NA,NA), warn_missing = FALSE))) %>% 
        mutate(
          #Background variables
          I06 = factor(I06, levels = 1:2, labels = c("Yes", "No")),
          I07 = factor(I07, levels = 1:2, labels = c("Yes", "No")),
          C01 = factor(C01, levels = 1:2, labels = c("Yes", "No")),
          C02 = factor(C02, levels = 1, labels = c("Agreed to and signed")),
          I04 = factor(I04, levels = 1:2, labels = c("Person with disability", "Person without disability")),
          I05 = factor(I05, levels = 1:2, labels = c("Male", "Female")),
          I08 = factor(I08, levels = 1:6, labels = c("Mother", "Father", "Grandparent", "Spouse", "Non-spouse", "Other")),
          I09 = factor(I09, levels = 1:7, labels = c("0-5", "6-12", "13-17", "18-24", "25-44", "45-64", "65+")),
          AGE = factor(plyr::mapvalues(I07, from = c("Yes", "No"), to = c("Child", "Adult")), levels = c("Child", "Adult")),
          #Health
          H01 = factor(H01, levels = 1:5, labels = c("Very good", "Good", "Neither good nor poor", "Poor", "Very poor")),
          H02 = factor(H02, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          H07 = factor(H07, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          H03 = factor(H03, levels = 1:2, labels = c("Yes", "No")),
          H13 = factor(H13, levels = 1:2, labels = c("Yes", "No")),
          H04 = factor(H04, levels = 1:5, labels = c("In the last year", "1-2 years ago", "Between 2-5 years ago", "Longer than 5 years ago", "Never")),
          H05 = factor(H05, levels = 1:3, labels = c("Yes, I was unable to get the care I needed", "No, I got the care I needed", "No need for health care in the past 12 months")),
          H08 = factor(H08, levels = 1:3, labels = c("Yes, I was unable to get the care I needed", "No, I got the care I needed", "No need for rehabilitation in the past 12 months")),
          H06 = factor(H06, levels = 1:13, labels = c("Health care facility too far away", "Could not afford the cost of the visit", "No transport available", "Transport not accessible", "Could not afford the cost of transport", "You were previously bad treated", "Could not take time off work or had other commitments", "Health care provider drugs or equipment were inadequate", "Health care providers skills were inadequate", "Did not know where to go", "Tried but were denied health care", "Thought you were not sick enough", "Other")),
          H09 = factor(H09, levels = 1:13, labels = c("Health care facility too far away", "Could not afford the cost of the visit", "No transport available", "Transport not accessible", "Could not afford the cost of transport", "You were previously bad treated", "Could not take time off work or had other commitments", "Health care provider drugs or equipment were inadequate", "Health care providers skills were inadequate", "Did not know where to go", "Tried but were denied health care", "Thought you were not sick enough", "Other")),
          H10 = factor(H10, levels = 1:5, labels = c("Yes and it works well", "Yes but it does not work or is not appropriate", "No, but I need it", "No, because it is broken or not appropriate", "No, I do not need it")),
          H11 = factor(H11, levels = 1:5, labels = c("Yes and it works well", "Yes but it does not work or is not appropriate", "No, but I need it", "No, because it is broken or not appropriate", "No, I do not need it")),
          H12 = factor(H12, levels = 1:5, labels = c("Yes and it works well", "Yes but it does not work or is not appropriate", "No, but I need it", "No, because it is broken or not appropriate", "No, I do not need it")),
          H06_1 = factor(`H06/1`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_2 = factor(`H06/2`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_3 = factor(`H06/3`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_4 = factor(`H06/4`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_5 = factor(`H06/5`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_6 = factor(`H06/6`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_7 = factor(`H06/7`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_8 = factor(`H06/8`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_9 = factor(`H06/9`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_10 = factor(`H06/10`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_11 = factor(`H06/11`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_12 = factor(`H06/12`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H06_13 = factor(`H06/13`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_1 = factor(`H09/1`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_2 = factor(`H09/2`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_3 = factor(`H09/3`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_4 = factor(`H09/4`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_5 = factor(`H09/5`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_6 = factor(`H09/6`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_7 = factor(`H09/7`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_8 = factor(`H09/8`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_9 = factor(`H09/9`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_10 = factor(`H09/10`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_11 = factor(`H09/11`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_12 = factor(`H09/12`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          H09_13 = factor(`H09/13`, levels = c(TRUE,FALSE), labels = c("Yes", "No")),
          #Education
          E01 = factor(E01, levels = c(1,3:10), labels = c("No schooling or never completed any grade", "Elementary education", "Secondary school", "Vocational education", "College", "University", "Post-graduate studies", "Professional training", "Other")),
          E02 = factor(E02, levels = 1:4, labels = c("Regular institutions", "Specialized institutions", "Home-schooling", "Other forms of education")),
          E03 = factor(E03, levels = 1:2, labels = c("Yes", "No")),
          E04 = factor(E04, levels = 1:2, labels = c("Yes", "No")),
          E05 = factor(E05, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          #Livelihood
          L01 = factor(L01, levels = 1:10, labels = c("Not working and looking for work", "Not working for wages and not looking for paid work", "Working for wages or salary with an employer (full- or part-time)", "Working for wages, but currently on sick leave for more than three months", "Self-employed or own-account worker", "Working as unpaid family member (for example, working in family business)", "Retired because of the health condition", "Retired due to age", "Early retirement", "Other")),
          L02 = factor(L02, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          L03 = factor(L03, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          L04 = factor(L04, levels = 1:2, labels = c("Yes", "No")),
          L05 = factor(L05, levels = 1:2, labels = c("Yes", "No")),
          L06 = factor(L06, levels = 1:2, labels = c("Yes", "No")),
          #Social
          S01 = factor(S01, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          S02 = factor(S02, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          S03 = factor(S03, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          S04 = factor(S04, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          S05 = factor(S05, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          S06 = factor(S06, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          S07 = factor(S07, levels = 1:2, labels = c("Yes", "No")),
          #Empowerment
          M01 = factor(M01, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          M02 = factor(M02, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          M03 = factor(M03, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          M04 = factor(M04, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          M07 = factor(M07, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          M05 = factor(M05, levels = 1:2, labels = c("Yes", "No")),
          M06 = factor(M06, levels = 1:3, labels = c("Yes", "No, but I would like to", "No, I don’t want")),
          #Quality of life
          Q01 = factor(Q01, levels = 1:5, labels = c("Very good", "Good", "Neither good nor poor", "Poor", "Very poor")),
          Q02 = factor(Q02, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          Q03 = factor(Q03, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          Q04 = factor(Q04, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          Q05 = factor(Q05, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          Q06 = factor(Q06, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely")),
          Q07 = factor(Q07, levels = 1:5, labels = c("Not at all", "A little", "Moderately", "Mostly", "Completely"))
        )
      
      return(df_uploading)
    }
  })
  
  # list of tables -----------
  list_tables <- reactive({
    
    if (!is.null(df_uploaded())) {
      list(
        table2_1 = tab_cbr(df_uploaded(), vars_indicators = c("AGE"), vars_demo = c("I04"), value = "n"), #FIX
        table2_2 = tab_cbr(df_uploaded(), vars_indicators = c("AGE"), vars_demo = c("I05", "I04"), value = "n"), #FIX
        table3 = tab_cbr(df_uploaded(), vars_indicators = "I05", vars_demo = c("I04")) %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = "I09", vars_demo = c("I04"))) %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = "E01", vars_demo = c("I04"))), # FIX
        table4_1 = tab_cbr(df_uploaded(), vars_indicators = c("H01"), vars_demo = c("I04"), AGE=="Adult", resp_values = 1:2) %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = c("H02"), vars_demo = c("I04"), AGE=="Adult", resp_values = 4:5)),
        table4_2 = tab_cbr(df_uploaded(), vars_indicators = c("H01"), vars_demo = c("I05", "I04"), AGE=="Adult", resp_values = 1:2, value = "prop") %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = c("H02"), vars_demo = c("I05", "I04"), AGE=="Adult", resp_values = 4:5, value = "prop")),
        table5_1 = tab_cbr(df_uploaded(), vars_indicators = c("H01"), vars_demo = c("I04"), AGE=="Child", resp_values = 1:2) %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = c("H02"), vars_demo = c("I04"), AGE=="Child", resp_values = 4:5)),
        table5_2 = tab_cbr(df_uploaded(), vars_indicators = c("H01"), vars_demo = c("I05", "I04"), AGE=="Child", resp_values = 1:2, value = "prop") %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = c("H02"), vars_demo = c("I05", "I04"), AGE=="Child", resp_values = 4:5, value = "prop")),
        table6_1 = tab_cbr(df_uploaded(), vars_indicators = c("H03"), vars_demo = c("I04"), resp_values = 1, AGE=="Adult"),
        table6_2 = tab_cbr(df_uploaded(), vars_indicators = c("H03"), vars_demo = c("I05", "I04"), resp_values = 1, AGE=="Adult", value = "prop"),
        table7_1 = tab_cbr(df_uploaded(), vars_indicators = c("H03"), vars_demo = c("I04"), resp_values = 1, AGE=="Child"),
        table7_2 = tab_cbr(df_uploaded(), vars_indicators = c("H03"), vars_demo = c("I05", "I04"), resp_values = 1, AGE=="Child", value = "prop"),
        table8_1 = tab_cbr(df_uploaded(), vars_indicators = c("H05"), vars_demo = c("I04"), AGE=="Adult"),
        table8_2 = tab_cbr(df_uploaded(), vars_indicators = c("H05"), vars_demo = c("I05", "I04"), AGE=="Adult", value = "prop"),
        table9_1 = tab_cbr(df_uploaded(), vars_indicators = c("H05"), vars_demo = c("I04"), AGE=="Child"),
        table9_2 = tab_cbr(df_uploaded(), vars_indicators = c("H05"), vars_demo = c("I05", "I04"), AGE=="Child", value = "prop"),
        table10 = tab_cbr(df_uploaded(), vars_indicators = paste0("H06_",1:5), vars_demo = c("I04"), value = "n", resp_values = 1),
        table11_1 = tab_cbr(df_uploaded(), vars_indicators = c("H08"), vars_demo = c("I04"), AGE=="Adult"),
        table11_2 = tab_cbr(df_uploaded(), vars_indicators = c("H08"), vars_demo = c("I05", "I04"), AGE=="Adult", value = "prop"),
        table12_1 = tab_cbr(df_uploaded(), vars_indicators = c("H08"), vars_demo = c("I04"), AGE=="Child"),
        table12_2 = tab_cbr(df_uploaded(), vars_indicators = c("H08"), vars_demo = c("I05", "I04"), AGE=="Child", value = "prop"),
        table13 = tab_cbr(df_uploaded(), vars_indicators = paste0("H09_",1:5), vars_demo = c("I04"), value = "n", resp_values = 1),
        table14 = tab_cbr(df_uploaded(), vars_indicators = c("H10", "H11", "H12"), vars_demo = "I04", AGE=="Adult") %>% 
          select(-contains("without")),
        table15 = tab_cbr(df_uploaded(), vars_indicators = c("H10", "H11", "H12"), vars_demo = "I04", AGE=="Child") %>% 
          select(-contains("without")),
        table16_1 = tab_cbr(df_uploaded(), vars_indicators = c("E01"), vars_demo = c("I04"), AGE=="Adult"),
        table16_2 = tab_cbr(df_uploaded(), vars_indicators = c("E01"), vars_demo = c("I05", "I04"), AGE=="Adult", value = "prop"),
        table17_1 = tab_cbr(df_uploaded(), vars_indicators = c("E01"), vars_demo = c("I04"), I09=="6-12", resp_values = 2),
        table17_2 = tab_cbr(df_uploaded(), vars_indicators = c("E01"), vars_demo = c("I05", "I04"), I09=="6-12", value = "prop", resp_values = 2),
        table18_1 = tab_cbr(df_uploaded(), vars_indicators = c("E01"), vars_demo = c("I04"), I09=="13-17", resp_values = 3),
        table18_2 = tab_cbr(df_uploaded(), vars_indicators = c("E01"), vars_demo = c("I05", "I04"), I09=="13-17", value = "prop", resp_values = 3),
        table19_1 = tab_cbr(df_uploaded(), vars_indicators = c("E02"), vars_demo = c("I04", "AGE"), resp_values = 1),
        table19_2 = tab_cbr(df_uploaded(), vars_indicators = c("E02"), vars_demo = c("I04", "I05", "AGE"), value = "prop", resp_values = 1),
        table20_1 = tab_cbr(df_uploaded(), vars_indicators = c("E04"), vars_demo = c("I04"), resp_values = 1),
        table20_2 = tab_cbr(df_uploaded(), vars_indicators = c("E04"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 1),
        table21_1 = tab_cbr(df_uploaded(), vars_indicators = c("L02"), vars_demo = c("I04"), resp_values = 4:5),
        table21_2 = tab_cbr(df_uploaded(), vars_indicators = c("L02"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5),
        table22_1 = tab_cbr(df_uploaded(), vars_indicators = c("L01"), vars_demo = c("I04"), resp_values = 5),
        table22_2 = tab_cbr(df_uploaded(), vars_indicators = c("L01"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 5),
        table23_1 = tab_cbr(df_uploaded(), vars_indicators = c("L01"), vars_demo = c("I04"), resp_values = 3),
        table23_2 = tab_cbr(df_uploaded(), vars_indicators = c("L01"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 3),
        table24_1 = tab_cbr(df_uploaded(), vars_indicators = c("L04"), vars_demo = c("I04"), resp_values = 1),
        table24_2 = tab_cbr(df_uploaded(), vars_indicators = c("L04"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 1),
        table25_1 = tab_cbr(df_uploaded(), vars_indicators = c("L05", "L06"), vars_demo = c("I04"), resp_values = 1),
        table25_2 = tab_cbr(df_uploaded(), vars_indicators = c("L05", "L06"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 1),
        table26_1 = tab_cbr(df_uploaded(), vars_indicators = c("S01"), vars_demo = c("I04"), resp_values = 4:5),
        table26_2 = tab_cbr(df_uploaded(), vars_indicators = c("S01"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5),
        table27_1 = tab_cbr(df_uploaded(), vars_indicators = c("S02"), vars_demo = c("I04"), resp_values = 4:5),
        table27_2 = tab_cbr(df_uploaded(), vars_indicators = c("S02"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5),
        table28_1 = tab_cbr(df_uploaded(), vars_indicators = c("S03"), vars_demo = c("I04"), resp_values = 4:5),
        table28_2 = tab_cbr(df_uploaded(), vars_indicators = c("S03"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5),
        table29_1 = tab_cbr(df_uploaded(), vars_indicators = c("S04"), vars_demo = c("I04"), resp_values = 4:5),
        table29_2 = tab_cbr(df_uploaded(), vars_indicators = c("S04"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5),
        table30_1 = tab_cbr(df_uploaded(), vars_indicators = c("S05"), vars_demo = c("I04"), resp_values = 4:5),
        table30_2 = tab_cbr(df_uploaded(), vars_indicators = c("S05"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5),
        table31_1 = tab_cbr(df_uploaded(), vars_indicators = c("S06"), vars_demo = c("I04"), resp_values = 4:5) %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = c("S07"), vars_demo = c("I04"), resp_values = 1)),
        table31_2 = tab_cbr(df_uploaded(), vars_indicators = c("S06"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5) %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = c("S07"), vars_demo = c("I05", "I04"), resp_values = 1)),
        table32_1 = tab_cbr(df_uploaded(), vars_indicators = c("M01", "M02"), vars_demo = c("I04"), resp_values = 4:5),
        table32_2 = tab_cbr(df_uploaded(), vars_indicators = c("M01", "M02"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5),
        table33_1 = tab_cbr(df_uploaded(), vars_indicators = c("M04"), vars_demo = c("I04"), resp_values = 4:5),
        table33_2 = tab_cbr(df_uploaded(), vars_indicators = c("M04"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5),
        table34_1 = tab_cbr(df_uploaded(), vars_indicators = c("M05"), vars_demo = c("I04"), resp_values = 1),
        table34_2 = tab_cbr(df_uploaded(), vars_indicators = c("M05"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 1),
        table35_1 = tab_cbr(df_uploaded(), vars_indicators = c("M06"), vars_demo = c("I04")),
        table35_2 = tab_cbr(df_uploaded(), vars_indicators = c("M06"), vars_demo = c("I05", "I04"), value = "prop"),
        table36_1 = tab_cbr(df_uploaded(), vars_indicators = c("Q01"), vars_demo = c("I04"), resp_values = 1:2) %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = paste0("Q0",2:7), vars_demo = c("I04"), resp_values = 4:5)),
        table36_2 = tab_cbr(df_uploaded(), vars_indicators = c("Q01"), vars_demo = c("I05", "I04"), value = "prop", resp_values = 1:2) %>% 
          bind_rows(tab_cbr(df_uploaded(), vars_indicators = paste0("Q0",2:7), vars_demo = c("I05", "I04"), value = "prop", resp_values = 4:5))
        
           )
    }
    
  })
  
  # status of upload ----------
  output$statusUpload <- renderText({
    
    if (is.null(df_uploaded())) print("No file uploaded yet")
    else print("Thank you for uploading a file.")
  })
  
  # status of table rendering
  output$statusTables <- renderText({
    if (is.null(list_tables())) print("Tables not yet created.")
    else print("Tables are printed.")
    
  })
  
  # make download button appear after data uploaded ----------
  output$ui.action <- renderUI({
    if (is.null(df_uploaded())) return()
    downloadButton("download", "Download tables")
  })
  
  # allow user to download tables -----------
  output$download <- downloadHandler(
    filename = function() {
      paste0("CBR_TABLES", ".xlsx")
    },
    content = function(file) {
      write_xlsx(list_tables(), file)
    }
  )

  
}

# Run the application 
shinyApp(ui = ui, server = server)

