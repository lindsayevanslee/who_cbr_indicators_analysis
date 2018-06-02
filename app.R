#load libraries ----------------
library(plyr)
library(tidyverse)
library(readxl)
library(openxlsx)
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
      
      df_uploading <- df_uploading %>%   mutate_all(funs(plyr::mapvalues(., from = c(777,888,999), to = c(NA,NA,NA), warn_missing = FALSE))) %>% 
        mutate(
          #Background variables
          I06 = factor(I06, levels = 1:2, labels = c("Yes","No")),
          I07 = factor(I07, levels = 1:2, labels = c("Yes","No")),
          C01 = factor(C01, levels = 1:2, labels = c("Yes","No")),
          C02 = factor(C02, levels = 1, labels = c("Agreed to and signed")),
          I04 = factor(I04, levels = 1:2, labels = c("Person with disability","Person without disability")),
          I05 = factor(I05, levels = 1:2, labels = c("Male","Female")),
          I08 = factor(I08, levels = 1:6, labels = c("Mother","Father", "Grandparent", "Spouse", "Non-spouse", "Other")),
          I09 = factor(I09, levels = 1:7, labels = c('0-5','6-12', '13-17', '18-24', '25-44', '45-64', '65+')),
          #Health
          H01 = factor(H01, levels = 1:5, labels = c("Very good","Good", "Neither good nor poor", "Poor", "Very poor")),
          H02 = factor(H02, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          H07 = factor(H07, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          H03 = factor(H03, levels = 1:2, labels = c("Yes","No")),
          H13 = factor(H13, levels = 1:2, labels = c("Yes","No")),
          H04 = factor(H04, levels = 1:5, labels = c('In the last year', '1-2 years ago', 'Between 2-5 years ago', 'Longer than 5 years ago', 'Never')),
          H05 = factor(H05, levels = 1:3, labels = c('Yes, I was unable to get the care I needed', 'No, I got the care I needed', 'No need for health care in the past 12 months')),
          H08 = factor(H08, levels = 1:3, labels = c('Yes, I was unable to get the care I needed', 'No, I got the care I needed', 'No need for rehabilitation in the past 12 months')),
          H06 = factor(H06, levels = 1:13, labels = c('Health care facility too far away', 'Could not afford the cost of the visit', 'No transport available', 'Transport not accessible', 'Could not afford the cost of transport','You were previously bad treated', 'Could not take time off work or had other commitments', 'Health care provider drugs or equipment were inadequate', 'Health care providers skills were inadequate', 'Did not know where to go', 'Tried but were denied health care', 'Thought you were not sick enough', 'Other')),
          H09 = factor(H09, levels = 1:13, labels = c('Health care facility too far away', 'Could not afford the cost of the visit', 'No transport available', 'Transport not accessible', 'Could not afford the cost of transport','You were previously bad treated', 'Could not take time off work or had other commitments', 'Health care provider drugs or equipment were inadequate', 'Health care providers skills were inadequate', 'Did not know where to go', 'Tried but were denied health care', 'Thought you were not sick enough', 'Other')),
          H10 = factor(H10, levels = 1:5, labels = c('Yes and it works well', 'Yes but it does not work or is not appropriate', 'No, but I need it', 'No, because it is broken or not appropriate', 'No, I do not need it')),
          H11 = factor(H11, levels = 1:5, labels = c('Yes and it works well', 'Yes but it does not work or is not appropriate', 'No, but I need it', 'No, because it is broken or not appropriate', 'No, I do not need it')),
          H12 = factor(H12, levels = 1:5, labels = c('Yes and it works well', 'Yes but it does not work or is not appropriate', 'No, but I need it', 'No, because it is broken or not appropriate', 'No, I do not need it')),
          `H06/1` = factor(`H06/1`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/2` = factor(`H06/2`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/3` = factor(`H06/3`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/4` = factor(`H06/4`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/5` = factor(`H06/5`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/6` = factor(`H06/6`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/7` = factor(`H06/7`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/8` = factor(`H06/8`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/9` = factor(`H06/9`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/10` = factor(`H06/10`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/11` = factor(`H06/11`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/12` = factor(`H06/12`, levels = 1:0, labels = c('Yes', 'No')),
          `H06/13` = factor(`H06/13`, levels = 1:0, labels = c('Yes', 'No')),
          #Education
          E01 = factor(E01, levels = c(1,3:10), labels = c('No schooling or never completed any grade', 'Elementary education', 'Secondary school', 'Vocational education', 'College', 'University', 'Post-graduate studies', 'Professional training', 'Other')),
          E02 = factor(E02, levels = 1:4, labels = c('Regular institutions', 'Specialized institutions', 'Home-schooling', 'Other forms of education')),
          E03 = factor(E03, levels = 1:2, labels = c("Yes","No")),
          E04 = factor(E04, levels = 1:2, labels = c("Yes","No")),
          E05 = factor(E05, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          #Livelihood
          L01 = factor(L01, levels = 1:10, labels = c('Not working and looking for work','Not working for wages and not looking for paid work','Working for wages or salary with an employer (full- or part-time)','Working for wages, but currently on sick leave for more than three months','Self-employed or own-account worker', 'Working as unpaid family member (for example, working in family business)', 'Retired because of the health condition', 'Retired due to age', 'Early retirement', 'Other')),
          L02 = factor(L02, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          L03 = factor(L03, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          L04 = factor(L04, levels = 1:2, labels = c("Yes","No")),
          L05 = factor(L05, levels = 1:2, labels = c("Yes","No")),
          L06 = factor(L06, levels = 1:2, labels = c("Yes","No")),
          #Social
          S01 = factor(S01, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          S02 = factor(S02, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          S03 = factor(S03, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          S04 = factor(S04, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          S05 = factor(S05, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          S06 = factor(S06, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          S07 = factor(S07, levels = 1:2, labels = c("Yes","No")),
          #Empowerment
          M01 = factor(M01, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          M02 = factor(M02, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          M03 = factor(M03, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          M04 = factor(M04, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          M07 = factor(M07, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          M05 = factor(M05, levels = 1:2, labels = c("Yes","No")),
          M06 = factor(M06, levels = 1:3, labels = c("Yes","No, but I would like to", "No, I don’t want")),
          #Quality of life
          Q01 = factor(Q01, levels = 1:5, labels = c("Very good","Good", "Neither good nor poor", "Poor", "Very poor")),
          Q02 = factor(Q02, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          Q03 = factor(Q03, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          Q04 = factor(Q04, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          Q05 = factor(Q05, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          Q06 = factor(Q06, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely")),
          Q07 = factor(Q07, levels = 1:5, labels = c("Not at all","A little", "Moderately", "Mostly", "Completely"))
        )
      
      return(df_uploading)
    }
  })
  
  # list of tables -----------
  list_tables <- reactive({
    
    if (!is.null(df_uploaded())) {
      list(Table1 = tab_cbr(df_uploaded(), 
                            vars_indicators = c("H01", "H02"),
                            vars_demo = c("I04", "I05"),
                            value = "prop",
                            resp_values = 4:5),
           Table2 = tab_cbr(df_uploaded(),
                            vars_indicators = paste0("S0",1:6),
                            vars_demo = "I04",
                            resp_values = 1:2))
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
      write.xlsx(list_tables(), file)
    }
  )

  
}

# Run the application 
shinyApp(ui = ui, server = server)

