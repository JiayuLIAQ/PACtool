library(shiny)
library(ggplot2)

#data change only once at the begenin
dt <- data.frame(x=runif(1000), y=runif(1000))

ui <- fluidPage(
  sliderInput("slider1","slider1", value=5, min=1, max=10),
  sliderInput("slider2","slider2", value=0.8, min=0, max=1),
  radioButtons("check1","check1",choices = c("red","blue","green")),
  actionButton("refreshColours","refreshColours"),
  plotOutput("rys")
)


server <- function(input,output){
  pp <- eventReactive(c(input$refreshColours,input$slider2,input$slider1),{
    ggplot(dt, aes(x,y)) + 
      geom_point(size=input$slider1, alpha=input$slider2, colour=isolate(input$check1))
  })
  output$rys <- renderPlot({
    pp()
  })
}

shinyApp(ui=ui, server=server)