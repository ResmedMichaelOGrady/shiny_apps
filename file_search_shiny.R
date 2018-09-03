setwd("R:/general/ForMichael/r_wsp/system_test_dashboard")
library(shiny)
library(lubridate)
library(purrr)
library(dplyr)
library(tidyr)
library(reshape2)
library(DT)
library(data.table)

ui <- fluidPage(
  title = "Search Multiple Overnight Folders From Same Date For Relevant Data",
  sidebarLayout(
    sidebarPanel(
      textInput("folder", "Please paste the EVT2-xxx_UAID folder path below"),
      textInput("date", "Please paste the date below in the following format e.g 240818"),
      actionButton(inputId = "update", label = "Generate/Update Table"),
      br(),
      br(),
      textInput("name", "Enter the name you wish to save the output table as"),
      textInput("dir", "Enter the filepath of the location where you would like to save the output"),
      actionButton("save_data", label = "Save Data")
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        id = "dataset",
        tabPanel("Overnight Recording Times", DT::dataTableOutput("mytable1"))
      )
    )
  )
)

server <- function(input, output) {
  
  df <- function() {
    
    date_folders <- list.dirs(path = input$folder, full.names = TRUE, recursive = FALSE)
    date_folders <- date_folders[grepl(input$date, date_folders)]
    sub_folders <- list.dirs(path = date_folders)
    sub_folders <- sub_folders[grepl("SleepData_Appendix", sub_folders)]
    file_list <- file.path(sub_folders, lapply(sub_folders, function(x) list.files(x, pattern = "DopplerSensor.csv")))
    file_list <- file_list[grepl("DopplerSensor.csv", file_list)]
    
    doppler_file <- do.call("rbind", lapply(file_list, function(x) {
      temp_data <- fread(x, col.names = c("date_time", "resp I", "move I", "resp Q", "move Q", "status flag")) %>% slice(c(1, n())) %>% select("date_time")
      temp_data$recording <- basename(x)
      temp_data$date_time <- temp_data$date_time %>% as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%S", tz = "Japan") %>%
        with_tz(tzone = "Europe/Dublin") %>% as.character()
      temp_data %>% group_by(recording) %>% mutate(x= paste0("date.", row_number())) %>% spread(x, date_time) %>%
        setNames(c("recording", "start_date/time", "end_date/time"))
    }))
    
    doppler_file$time_diff <- as.POSIXct(doppler_file$`end_date/time`, format = "%Y-%m-%d %H:%M:%S") - as.POSIXct(doppler_file$`start_date/time`, format = "%Y-%m-%d %H:%M:%S")
    units(doppler_file$time_diff) <- c("hours")
    doppler_file$time_diff <- doppler_file$time_diff %>% round(digits = 2) %>% as.character()
    doppler_file
  }
    
  df2 <- reactiveValues()
  observeEvent(input$update, {
    showModal(modalDialog(p("Loading...Please Wait"),
                          title = "Loading"
                          
    ))
    df2$data <- df()
    showModal(modalDialog(p("Overnight Recordings Generated"),
                          title = "Exit"
                          
    ))
  })
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(df2$data)
  })
  
  observeEvent( input$save_data, {
    write.csv(df2$data, file = file.path(input$dir,paste(input$name ,".csv", sep = "")))
    showModal(modalDialog(p("Output Table Saved"),
                          title = "Exit"
    ))
  })
}

shinyApp(ui, server)