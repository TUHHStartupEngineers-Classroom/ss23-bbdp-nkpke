# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

source(file = "stock_analysis_functions.R")


# UI ----
ui <- fluidPage(
    title = "Stock Analyzer",
    
    # 1.0 HEADER ----
    div(
        h1("Stock Analzer"),
        p("This is my second shiny project")
    ),


    # 2.0 APPLICATION UI -----
div(
  column(
    width = 4,
    wellPanel(
    # index selection
    # title
    h4("Select Stock Index"),
    # picker
    pickerInput(inputId = "index_selection", choices = c("DOW", "DAX", "SP500", "NASDAQ")),
    # stock selection
    uiOutput("indices"), # only stock from selected index are shown (computation in server)
    # data range selection
    dateRangeInput(inputId = "date_range", label = "Date Range", start = today() - days(180), end = today()),
    # Add analyze button
    actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
    # reactive output
    p(textOutput(outputId = "selected_symbol")),
    # short moving average input slider
    sliderInput(inputId = "mavg_short", label = "Short Moving Average", min = 5, max = 30, value = 20),
    # long moving average input slider
    sliderInput(inputId = "mavg_long", label = "Long Moving Average", min = 30, max = 100, value = 50),
    ),
  ), 
  column(
    width = 8,
    div(
    h4(textOutput(outputId = "plot_title")),
    ),
    div(
        # Reactive output stock_data:
        verbatimTextOutput(outputId = "stock_data"),

        # plot reactive
        plotlyOutput(outputId = "plotly_plot"),

        # commentary Reactive
        p(textOutput(outputId = "commentary"))
    )
  )
)

)


# SERVER ----
server <- function(input, output, session) {
      # Get Stock List
    stock_list_tbl <- eventReactive(input$index_selection, {
      get_stock_list(input$index_selection)
      
    })
    
    # Stock Symbol ----
    stock_symbol <- eventReactive(input$analyze, {
      input$stock_selection 
    })
    
    output$selected_symbol <- renderText({stock_symbol()})
    output$plot_header <- renderText(stock_symbol())
  
    # Create stock list ----    
    output$indices <- renderUI({
        choices = stock_list_tbl() %>% purrr::pluck("label")
        pickerInput(inputId = "stock_selection", label = "Select Stock", choices = choices)})
    # Stock symbol the user has selected
    stock_symbol <- eventReactive(input$analyze, {
        input$stock_selection %>% 
            get_symbol_from_user_input()
    })
    output$selected_symbol <- renderText({stock_symbol()})

    # Update plot title to stock selection, do not ignore null
    stock_selection <- reactive({
        input$stock_selection
    })
    output$plot_title <- renderText({stock_selection()})

    # Get Stock Data ----
    stock_data_tbl <- reactive({
      stock_symbol() %>% 
            get_stock_data(from = input$date_range[1],
                               to = input$date_range[2],
                               mavg_short = input$mavg_short,
                               mavg_long = input$mavg_long)})

    output$stock_data <- renderPrint({stock_data_tbl()})

    # Reactively Render the Interactive Time Series Plot - On Stock Data Update
    output$plotly_plot <- renderPlotly({
        stock_data_tbl() %>% plot_stock_data()
    })
    
    # Generate Commentary ----
    output$commentary <- renderText({
        stock_data_tbl() %>% 
            generate_commentary(user_input = stock_symbol())
    })
  


}

# RUN APP ----
shinyApp(ui = ui, server = server)
