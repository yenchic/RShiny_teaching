library(shiny)
library(TDA)
library(KernSmooth)
# Define UI for application
ui <- shinyUI(fluidPage(

  selectInput(inputId = "type",
              label = "Distribution",
              choices = c("Circle", "Three Circles", "Gaussian Mixture"),
              selected = "Circle"),
  
  numericInput(inputId = "n", 
               label = "Sample Size", 
               500),
  sliderInput(inputId = "err",
              label = "Noise level",
              min = 0.025, max = 1, value = 0.1, step = 0.025),
  
  numericInput(inputId = "range", 
               label = "Range of diagram", 
               2),
  
  sliderInput(inputId = "bw_adjust",
              label = "Smoothing bandwidth",
              min = 0.025, max = 1, value = 0.1, step = 0.025),
  
  actionButton("go", "New Sample"),
  
  checkboxInput(inputId = "auto",
                label = "Automatically generating",
                value = FALSE),
  

  plotOutput(outputId = "main_plot", height = "450px", width="900px")
))

# Define server logic
server <- shinyServer(function(input, output, session) {
  data_fn1 = function(N, err){
    mu0 = c(0.5, 0.5)
    ### generate data
    U = runif(N, min=0, max=2*pi)
    X0 = cbind(cos(U)+mu0[1], sin(U)+mu0[2])
    X = X0 + matrix(rnorm(2*N, sd= err), nrow=N)
    return(X)
  }
  data_fn2 = function(N, err){
    mu1 = c(0,0)
    mu2 = c(4,0)
    mu3 = c(2, 4)
    ### generate data
    U = runif(N, min=0, max=2*pi)
    X0 = cbind(cos(U), sin(U))
    U1 = X0 + matrix(rnorm(2*N, sd= err), nrow=N)
    
    I1 = floor(runif(N)*3)
    U1[which(I1==0),] = t(t(U1[which(I1==0),]) + mu1)
    U1[which(I1==1),] = t(t(U1[which(I1==1),]) + mu2)
    U1[which(I1==2),] = t(t(U1[which(I1==2),]) + mu3)
    X = U1
    return(X)
  }
  data_fn3 = function(N, err){
    x1 = c(0,0)
    x2 = c(0.8,0)
    x3 = c(0.4, 1.2)
    x4 = c(1.2, 1.2)
    X0 = rbind(x1,x2,x3)
    ### generate data
    I1 = floor(runif(N)*4)
    U1 = matrix(rnorm(n=N*2, sd=err), ncol=2)
    U1[which(I1==0),] = t(t(U1[which(I1==0),]) + x1)
    U1[which(I1==1),] = t(t(U1[which(I1==1),]) + x2)
    U1[which(I1==2),] = t(t(U1[which(I1==2),]) + x3)
    U1[which(I1==3),] = t(t(U1[which(I1==3),]) + x4)
    X = U1
    return(X)
  }
  
  randomVals <- eventReactive(input$go, {
    if(input$type=="Circle")
      data_fn1(input$n,input$err)
    else if(input$type=="Three Circles")
      data_fn2(input$n,input$err)
    else if(input$type=="Gaussian Mixture")
      data_fn3(input$n,input$err)
  })
  
  autoInvalidate <- reactiveTimer(1000, session)  
  output$main_plot <- renderPlot({
    dat = randomVals()
    
    if(input$type=="Circle")
      xylim0 = c(-1.5,2.5)
    else if(input$type=="Three Circles")
      xylim0 = c(-2,6)
    else if(input$type=="Gaussian Mixture")
      xylim0 = c(-1,2)
    
    if(input$auto){
      autoInvalidate()
      if(input$type=="Circle")
        dat = data_fn1(input$n,input$err)
      else if(input$type=="Three Circles")
        dat = data_fn2(input$n,input$err)
      else if(input$type=="Gaussian Mixture")
        dat = data_fn3(input$n,input$err)
    }
    
    par(mfrow=c(1,2))
      # plotting
    D_kde = bkde2D(dat, bandwidth = input$bw_adjust,
                   gridsize = c(101,101), 
                   range.x=list(xylim0,xylim0))
    colP = colorRampPalette(c("white","orange","brown"))
    image(D_kde$x1,D_kde$x2,D_kde$fhat,
            ylab="", xlab="X",
            main="Density Contour", 
            cex.lab=2, cex.main=2, cex.axis=2, col=colP(50))
    points(dat, pch=20, col="black")
    mtext("Y", side=2, line=2.6, cex=2)
    PD = gridDiag(dat,FUN = kde,h=input$bw_adjust,
                  lim = cbind(xylim0,xylim0),sublevel = F,
                  by = 0.05)
    
    plot(NULL, xlim=c(0,input$range), ylim=c(0,input$range),
         xlab="Death Time", ylab="",main="Persistent Diagram", 
         cex.lab=2, cex.main=2, cex.axis=2)
    abline(a=0,b=1)
    mtext("Birth Time", side=2, line=2.6, cex=2)
    if(length(which(PD$diagram[,1]==1)==1)){
      points(x=PD$diagram[which(PD$diagram[,1]==0),2],
             y=PD$diagram[which(PD$diagram[,1]==0),3], pch=16)
    }
    else if(length(which(PD$diagram[,1]==1)>1)){
      points(PD$diagram[which(PD$diagram[,1]==0),2:3], pch=16)
    }
    if(length(which(PD$diagram[,1]==1)==1)){
      points(x=PD$diagram[which(PD$diagram[,1]==1),2],
             y=PD$diagram[which(PD$diagram[,1]==1),3], pch=2, col="red",
             lwd=3)
    }
    else if(length(which(PD$diagram[,1]==1)>1)){
      points(PD$diagram[which(PD$diagram[,1]==1),2:3], pch=2, col="red",
             lwd=3)
    }
  })
  
})
# Run the application 
shinyApp(ui = ui, server = server)

