##### OPTION 1 ############################################################################################

library(shiny)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("integer", "Integer:",
                  min = 1986, max = 2016,
                  step = 5,
                  sep = "",
                  value = 2016),
      
      # Input: Specification of range within an interval ----
      sliderInput("range", "Range:",
                  min = 1986, max = 2016,
                  step = 5,
                  sep = "",
                  value = c(2006, 2016))
      
      shinyWidgets::materialSwitch(
        inputId = NS(id, "integer"), label = i18n$t("250-metre grid"), 
        status = "primary", value = TRUE))
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Table summarizing the values entered ----
    tableOutput("values")
    
  )
)
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Integer",
               "Range"),
      Value = as.character(c(input$integer,
                             paste(input$range, collapse = " "))),
      stringsAsFactors = FALSE)
    
  })
  
  df <- reactive({if (input$integer) "integer" else input$range})
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}

shinyApp(ui, server)


#### OPTION 2 ############################################################################

library(shiny)

slider_min <- 1986
slider_max <- 2016
slider_init <- c(2006, 2016)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("integer", "Integer:",
                  min = slider_min, max = slider_max,
                  step = 5,
                  sep = "",
                  value = 2016),
      sliderInput("range", "Range:",
                  min = slider_min, max = slider_max,
                  step = 5,
                  sep = "",
                  value = c(2006, 2016)),
      checkboxInput("checkbox", "Comparative mode")
    ),
    
    mainPanel()
  )
)

server <- function(input, output, session) {
  
  slider_values <- reactiveVal(slider_init)
  
  observe({
    if(input$checkbox){
      slider_values(isolate(input$range))
      updateSliderInput(session, "range", value=c(2006, 2016))
    }else{
      updateSliderInput(session, "integer", value=2016)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
