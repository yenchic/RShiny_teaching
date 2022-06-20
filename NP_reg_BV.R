library(shiny)

# Define UI for application
ui <- shinyUI(fluidPage(
  
  selectInput(inputId = "color",
              label = "Color of the histogram:",
              choices = c("Red", "Orchid","Limegreen"),
              selected = "Red"),
  
  selectInput(inputId = "n",
              label = "Sample size:",
              choices = c(250, 500, 1000, 2000, 5000),
              selected = 500),

  sliderInput(inputId = "bw_adjust",
              label = "Bandwidth adjustment:",
              min = 0.025, max = 1, value = 0.5, step = 0.025),
  
  sliderInput(inputId = "err",
              label = "Noise level:",
              min = 0.025, max = 1, value = 0.25, step = 0.025),
  
  plotOutput(outputId = "main_plot", height = "300px")
                   
))

# Define server logic
server <- shinyServer(function(input, output, session) {
  autoInvalidate <- reactiveTimer(1000, session)  
  output$main_plot <- renderPlot({
    autoInvalidate()
      # true curve
    x_seq <- seq(from=0, to=3*pi, length.out=1000)
    d_seq <- 2*exp(-0.1*x_seq)*sin(4*x_seq)
      # data
    X = runif(input$n, min=0, max=3*pi)
    Y = 2*exp(-0.1*X)*sin(4*X)+rnorm(input$n, sd=input$err)
      # plotting
    plot(x=X, y=Y, pch=20, col="gray",
         ylim=c(-3,3), ylab="", xlab="X",
         main="Kernel Regression", 
         cex.lab=2, cex.main=2, cex.axis=2, xlim=c(0,3*pi))
    mtext("Y", side=2, line=2.6, cex=2)
    lines(x=x_seq, y=d_seq, type="l", lwd=6, col="dodgerblue")
    Kreg = ksmooth(X,Y, kernel="normal",bandwidth = input$bw_adjust)
    lines(Kreg, lwd=6, col=input$color)
    legend("bottomleft",c("Theory","Estimated"), 
           col=c("dodgerblue", input$color),
           lwd=6, cex=1.5)
  })
})
# Run the application 
shinyApp(ui = ui, server = server)

