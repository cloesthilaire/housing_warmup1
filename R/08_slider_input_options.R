#### OPTION 1 ############################################################################

library(shiny)

slider_min <- 1986
slider_max <- 2016
slider_init <- c(2006, 2016)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.checkbox==0",
        sliderInput("integer", "Integer:",
                  min = slider_min, max = slider_max,
                  step = 5,
                  sep = "",
                  value = 2016)
        ),
      conditionalPanel(
        condition = "input.checkbox==1",
        sliderInput("range", "Range:",
                  min = slider_min, max = slider_max,
                  step = 5,
                  sep = "",
                  value = c(2006, 2016)
                  )
        ),
      checkboxInput("checkbox", "Compare between years", FALSE)
    ),
    
    mainPanel()
  )
)

server <- function(input, output, session) {
  
  slider_values <- reactiveVal(slider_init)
  
  observe({
    if(input$checkbox == 1){
      slider_values(isolate(input$range))
      updateSliderInput(session, "range", value=c(2006, 2016))
    }else{
      updateSliderInput(session, "integer", value=2016)
    }
    #outputOptions(output, "slider_values", suspendWhenHidden = FALSE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
