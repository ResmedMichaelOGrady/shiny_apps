setwd("R:/general/ForMichael/r_wsp/system_test_dashboard")
library(shiny)
library(lubridate)
library(purrr)
library(dplyr)
library(tidyr)
library(reshape2)
library(DT)
library(data.table)
library(ggplot2)

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
      actionButton("save_data", label = "Save Data"),
      actionButton("save_plot", label = "Save Plot")
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        id = "dataset",
        tabPanel("Overnight Recording Times", DT::dataTableOutput("mytable1")),
        tabPanel("Timeline Plot", plotOutput("timeline_plot"))
      )
    )
  )
)

server <- function(input, output) {
  
  df <- function() {
    
    date_folders <- list.dirs(path = input$folder, full.names = TRUE, recursive = FALSE)
    date_folders <- date_folders[grepl(input$date, date_folders)]
    
    file_list <- file.path(date_folders, lapply(date_folders, function(x) list.files(x, pattern = "readme.csv")))
    
    doppler_file <- do.call("rbind", lapply(file_list, function(x) {
      temp_data <- read.csv(x, col.names = c("start_date_time", "end_date_time")) 
      temp_data$start_date_time <- temp_data$start_date_time %>% as.POSIXct(start_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Dublin") %>%
        as.character()
      temp_data$end_date_time <- temp_data$end_date_time %>% as.POSIXct(end_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Dublin") %>%
        as.character()
      temp_data$recording <- basename(x)
      temp_data
    }))
    
    doppler_file$recording <- basename(tools::file_path_sans_ext(date_folders))
    doppler_file$time_diff <- as.POSIXct(doppler_file$`end_date_time`, format = "%Y-%m-%d %H:%M:%S") - as.POSIXct(doppler_file$`start_date_time`, format = "%Y-%m-%d %H:%M:%S")
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
  
  output$timeline_plot <- renderPlot({
    ggplot(df2$data) + geom_rect(aes(xmin = as.POSIXct(`start_date_time`, format = "%Y-%m-%d %H:%M:%S"), 
                                     xmax = as.POSIXct(`end_date_time`, format = "%Y-%m-%d %H:%M:%S"), 
                                     ymin = `recording`, ymax = `recording`, color = `recording`), size = 5) +
      scale_x_datetime(date_breaks = "3 hours")
  })
  
  observeEvent( input$save_data, {
    write.csv(df2$data, file = file.path(input$dir,paste(input$name ,".csv", sep = "")))
    showModal(modalDialog(p("Output Table Saved"),
                          title = "Exit"
    ))
  })
  
  plot_data <- reactive ({
    ggplot(df2$data) + geom_rect(aes(xmin = as.POSIXct(`start_date_time`, format = "%Y-%m-%d %H:%M:%S"), 
                                                 xmax = as.POSIXct(`end_date_time`, format = "%Y-%m-%d %H:%M:%S"), 
                                                 ymin = `recording`, ymax = `recording`, color = `recording`), size = 5) +
    scale_x_datetime(date_breaks = "3 hours")
  })
  
  observeEvent( input$save_plot, {
    ggsave(filename = paste(input$name, ".png", sep = ""), plot = plot_data(), path = input$dir)
    showModal(modalDialog(p("Timeline Plot Saved"),
                          title = "Exit"
    ))
  })
  
}

shinyApp(ui, server)