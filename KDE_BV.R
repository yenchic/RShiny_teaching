#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
ui <- shinyUI(fluidPage(
  
  selectInput(inputId = "color",
              label = "Color of the estimator:",
              choices = c("Red", "Orchid","Limegreen"),
              selected = "Red"),
  
  selectInput(inputId = "n",
              label = "Sample size:",
              choices = c(250, 500, 1000, 2000, 5000),
              selected = 500),
  
  checkboxInput(inputId = "pts",
                label = "Show data point",
                value = FALSE),
  
  sliderInput(inputId = "bw_adjust",
              label = "Bandwidth adjustment:",
              min = 0.025, max = 1, value = 0.5, step = 0.025),
  
  plotOutput(outputId = "main_plot", height = "300px")
                   
))

# Define server logic
server <- shinyServer(function(input, output, session) {
  autoInvalidate <- reactiveTimer(1000, session)  
  output$main_plot <- renderPlot({
    autoInvalidate()
    x_seq <- seq(from=-3, to=3, length.out=500)
    d_seq <- dnorm(x_seq)
    plot(x=x_seq, y=d_seq, type="l", lwd=6,
         ylim=c(0,0.5), ylab="", xlab="X",
         main="Bias-Variance Tradeoff", col="blue",
         cex.lab=2, cex.main=2, cex.axis=2)
    mtext("Density", side=2, line=2.6, cex=2)
    abline(h=0, col="gray")
    data <- rnorm(input$n)
    lines(density(data, bw=input$bw_adjust,
                  from=-3, to=3), lwd=6, col=input$color)
    legend("topleft",c("Theory","Estimated"), col=c("blue", input$color),
           lwd=6, cex=1.5)
    if(input$pts){
     options(warn=-1)
     rug(data)
    }
  })
})
# Run the application 
shinyApp(ui = ui, server = server)

