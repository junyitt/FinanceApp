library(shiny)
library(DT)
library(plotly)
source("./function.R")

ui <- fluidPage(
  titlePanel("Financial Calculator"),
  
  #sidebarLayout(
  #sidebarPanel(
  helpText("Help you calculate your expenses"),
  
  selectInput("var", 
              label = "Choose your expenses",
              choices = c("Petrol", 
                          "Food",
                          "Rental", 
                          "Loan"),
              selected = "Food"),
  numericInput("age",
               "Input your current age:",
               value = 20),
  selectInput("age_m", 
              label = "How many months old? 0 represents 12",
              choices = c(0:11),
              selected = 0),
    numericInput("init_income",
               "Enter your initial income:",
               value = 3000),
  numericInput("growth_rate",
               "Enter the expected income growth rate in %:",
               value = 5),
  numericInput("growth_duration",
               "Duration needed for each increments in years:",
               value = 2),
  numericInput("promo_rate",
               "Enter your increment rate for promotion:",
               value = 10),
  numericInput("promo_time",
               "Duration for promotion:",
               value = 5),
  numericInput("numInput",
               "Enter your yearly saving at the end of the month
               in RM:",
               value = 1000),
  
  numericInput("AIR",
               "Enter the current interest rate in %:",
               value = 5),
  sliderInput("range", 
              label = "Years of Saving:",
              min = 1, max = 50, value = 10),
  DT::dataTableOutput("mytable"),
  hr(),
  actionButton("action","Saving Projection"),
  
  
  #),
  
  #mainPanel(
  textOutput("selected_var"),
  textOutput("txtOutput"),
  textOutput("AIRout"),
  textOutput("FV"),
  textOutput("init_income_out"),
  textOutput("growth_rate_out"),
  textOutput("duration_out"),
  plotOutput("plot_saving"),
  plotOutput("plot_income"),
  plotOutput("timeseries")
  
  #)
  #)
  #)
)

server <- function(input, output) {

  res<-reactiveValues(df=data.frame())
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })
  output$txtOutput <- renderText({
    paste("Your yearly saving is RM: ", input$numInput)
  })
  output$FV <- renderText({
    paste("Your future value for",input$range,"years is RM: ", as.numeric(future_val(input$numInput,input$AIR,input$range)))
  })
  output$AIRout <- renderText({
    paste("Your annual rate of return is: ", input$AIR,"%")
  })
  output$init_income_out <- renderText({ 
    paste("Your initial income is RM", input$init_income)
  })
  output$growth_rate_out <- renderText({ 
    paste("Your income growth rate is", input$growth_rate,"%")
  })
  output$duration_out <- renderText({ 
    paste("Your income growth rate is", input$growth_duration,"%")
  })
  v <- reactiveValues(data = NULL)
  observeEvent(input$action, {
    v$data <- saving_proj(input$numInput,input$AIR,input$range)
    v$incomedata <- income_proj(input$init_income,input$growth_rate,input$growth_duration,input$range)
    res$df <- final_df(as.numeric(input$age),as.numeric(input$range),as.numeric(input$age_m),as.numeric(input$growth_rate),as.numeric(input$growth_duration),as.numeric(input$promo_rate),as.numeric(input$promo_time),as.numeric(input$init_income))
    #input$init_income,input$growth_rate,input$growth_duration,input$range
  })
  output$mytable = DT::renderDataTable({
    # final_df(as.numeric(input$age),as.numeric(input$range),as.numeric(input$age_m),as.numeric(input$growth_rate),as.numeric(input$growth_duration),as.numeric(input$promo_rate),as.numeric(input$promo_time),as.numeric(input$init_income))
    res$df
  })
  output$plot_saving <- renderPlot({
    if (is.null(v$data)) return()
    plot(x=c(0:(length(v$data)-1)),y=v$data,type='l',ylab='Savings in RM',xlab='Year',main=paste('Projection of Savings over',input$range,' years'))
  })
  output$plot_income <- renderPlot({
    if (is.null(v$incomedata)) return()
    
    # result$date<-reactiveValues(res$date)
    # result$income_p<-reactiveValues(res$proj_income)
    df <- res$df
    plot_ly(x=df$date,y=df$proj_income,mode='lines',type='scatter')
  # plot(x=df$date,y=df$proj_inc,type='l',ylab='Income Projection in RM',xlab='Age_month',main=paste('Projection of Income over',input$range,' years'))
  })

  
}

shinyApp(ui, server)