library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE), selectInput("dataset", "Choose a dataset:",
                                                           choices = c("Select csv")),selectInput("date", "Choose a data:",
                     choices = c("Select csv")),
      
      fileInput("file2", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),column(8, wellPanel(
        verbatimTextOutput("urlText")
      )),column(8, wellPanel(
        verbatimTextOutput("urlText2")
      ))
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("contents"),
      plotOutput("plot1")
    )
  )
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.delim(inFile$datapath, header = input$header, encoding  = "UTF8", stringsAsFactors =FALSE)
    
    df <- data.frame(data)
    
    df$date <- ISOdate(data$"Год",data$"Месяц",1)
    df$"Количество.пациентов.на.учете"[is.na(df$"Количество.пациентов.на.учете")] <- 0
    
    
    index2replace_region <-  ! ((df$Наименование.региона == "Москва") |
                                  (df$Наименование.региона == "Московская область") |
                                  (df$Наименование.региона == "Санкт-Петербург и Ленинградская область"))
    
    df[index2replace_region,]$Наименование.региона  <- "Другие"
    df2plot <- df %>%
      #select('Наименование.региона','Количество.пациентов.на.учете','date')
      select(2,6,7)
    p<-ggplot(data=df2plot, aes(x=date, y=Количество.пациентов.на.учете,fill=Наименование.региона)) +
      geom_bar(stat="identity") +
      labs(title="Динамика пациентов по вермени",
           x ="Время", y = "Кол-во пациентов на учёте")
    p
  })


  
  
  
  #output$contents <- renderTable({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  #  inFile <- input$file1
  #
  #  if (is.null(inFile))
  #    return(NULL)
  #  
  #  read.delim(inFile$datapath, header = input$header, encoding  = "UTF8")
  #})
  
  output$plot <- renderPlot({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.delim(inFile$datapath, header = input$header, encoding  = "UTF8", stringsAsFactors =FALSE)
    
    df <- data.frame(data)
    df$"Количество"[is.na(df$"Количество")] <- 0
    
    
    category_df <- df[df$"Категория"==input$dataset,]
    category_df$date <- ISOdate(category_df$"Год",category_df$"Месяц",1)
    category_df$datesum <- format(as.Date(category_df$date), "%Y-%m");
    unique_element <- unique(category_df$Тип.помощи)
    
    category_df_date <- category_df[category_df$datesum==input$date,]
    df2plot <- category_df_date %>%
      #select('Наименование.региона','Количество.пациентов.на.учете','date')
      select(1,5)
    p<-ggplot(data=df2plot, aes(x=Тип.помощи, y=Количество,fill=Тип.помощи)) +
      geom_bar(stat="identity") +
      geom_text( aes(label = Количество),
                 position = position_dodge(0.9),
                 vjust = 0)
      labs(title="Динамика пациентов по вермени",
           x ="Время", y = "Кол-во пациентов на учёте")
    p
    
    
  })
  
  output$urlText <- renderText({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.delim(inFile$datapath, header = input$header, encoding  = "UTF8", stringsAsFactors =FALSE)
    
    df <- data.frame(data)
    
    df$"Количество"[is.na(df$"Количество")] <- 0
    unique_element <- unique(df$Категория)
    
    
    updateSelectInput(session, "dataset",
                      label = paste("Select input label", length(unique_element)),
                      choices = unique_element,
                      selected = tail(unique_element, 1)
    
    )
    
    return(NULL)
    
   
   
    
  })
  output$urlText2 <- renderText({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.delim(inFile$datapath, header = input$header, encoding  = "UTF8", stringsAsFactors =FALSE)
    
    df <- data.frame(data)
    
    df$"Количество"[is.na(df$"Количество")] <- 0
    unique_element <- unique(df$Категория)
    
    
    
    
    
    
    category_df <- df[df$"Категория"==input$dataset,]
    unique_element <- unique(category_df$Тип.помощи)
    category_df$date <- ISOdate(category_df$"Год",category_df$"Месяц",1)
    unique_element_data <- unique(format(as.Date(category_df$date), "%Y-%m"))
    updateSelectInput(session, "date",
                      label = paste("Select input label", length(unique_element_data)),
                      choices = unique_element_data,
                      selected = tail(unique_element_data, 1)
    )
    return(NULL)
    
  })

  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.delim(inFile$datapath, header = input$header, encoding  = "UTF8", stringsAsFactors =FALSE)
    
    df <- data.frame(data)
    df$"Количество"[is.na(df$"Количество")] <- 0
  
   
    category_df <- df[df$"Категория"==input$dataset,]
    category_df$date <- ISOdate(category_df$"Год",category_df$"Месяц",1)
    category_df$datesum <- format(as.Date(category_df$date), "%Y-%m");
    unique_element <- unique(category_df$Тип.помощи)
  
    category_df_date <- category_df[category_df$datesum==input$date,]

    category_df_date

  })
}

shinyApp(ui = ui, server = server)