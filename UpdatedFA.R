library(shiny)
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
      numericInput("numInput",
                   "Enter your yearly saving at the end of the month
                   in RM:",
                   value = 1000),
      numericInput("AIR",
                   "Enter the current interest rate in %:",
                   value = 5),
      sliderInput("range", 
                 label = "Years of Saving:",
               min = 1, max = 50, value = 30),
      hr(),
      actionButton("action","Saving Projection"),
      
    #),
    
    #mainPanel(
    textOutput("selected_var"),
    textOutput("txtOutput"),
    textOutput("AIRout"),
    textOutput("FV"),
    plotOutput("plot")
      
    #)
  #)
#)
)

server <- function(input, output) {

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
  v <- reactiveValues(data = NULL)
  observeEvent(input$action, {
    v$data <- seq(0,input$range,by=2)
  })
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })
  
}

shinyApp(ui, server)

