

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    
    NoOfSamples <- input$NoOfSamples
    set.seed(101)
    X=rnorm(NoOfSamples)
    

    hist(main="Sample distribution from ~N(0,1)",X, col = 'darkgray', border = 'white',freq=F,xlab=NULL,breaks=20)
    curve(dnorm(x),add=T)
  })
  
  
  output$distPlot2 <- renderPlot({
    
    
    SampleSize <- input$SampleSize
    set.seed(101)
    X=sapply(1:1000,FUN=function(i)mean(rnorm(SampleSize)))
    
    par(mfrow=c(1,2))
    curve(main=paste("Standard Normal Distribution"),dnorm(x),from=-3,to=3,col="blue",xlab="",ylab="Density")
    hist(main=paste("Distribution of 1000 sample mean (n=",SampleSize,")"),X, col = 'blue', border = 'white',freq=F,xlab=NULL,breaks=30)
    curve(dnorm(x,mean=0,sd=1/sqrt(SampleSize)),add=T)
  })
  
  output$distPlot3 <- renderPlot({
    
    
    SampleSize_unif <- input$SampleSize_unif
    set.seed(101)
    X=sapply(1:1000,FUN=function(i)mean(runif(SampleSize_unif)))
    
    par(mfrow=c(1,2))
    curve(main=paste("Uniform Distribution (min=0, max=1)"),dunif(x),col="red",xlab="",ylab="Density")
    hist(main=paste("Distribution of 1000 sample mean (n=",SampleSize_unif,")"),X, col = 'red', border = 'white',freq=F,xlab="",breaks=30)
    curve(dnorm(x,mean=0.5,sd=((1-0)/sqrt(12))/sqrt(SampleSize_unif)),add=T)
  })
  
  output$distPlot4 <- renderPlot({
    
    
    SampleSize_pois <- input$SampleSize_pois
    set.seed(101)
    X=sapply(1:1000,FUN=function(i)mean(rpois(SampleSize_pois,lambda = 3)))
    
    par(mfrow=c(1,2))
    x=1:10
    plot(main=paste("Poisson Distribution (lambda=3)"),x=x,y=3*x*exp(-3)/factorial(x),type="o",col="green",xlab="",ylab="Density")
    hist(main=paste("Distribution of 1000 sample mean (n=",SampleSize_pois,")"),X, col = 'green', border = 'white',freq=F,xlab="",breaks=30)
    curve(dnorm(x,mean=3,sd=sqrt(3)/sqrt(SampleSize_pois)),add=T)
  })
  
  output$distPlot5 <- renderPlot({
    
    
    sample_size <- input$sample_size
    sample_mean <- input$sample_mean
    z=(sample_mean-10)/(3/sqrt(sample_size))
    curve(main="Standard Normal Distribution",dnorm(x),from=-5,to=5,xlab=paste("p-value=",round(2*(1-pnorm(abs(z))),3)),ylab="Density") 

    
    abline(v=z,col="grey",lty=2)   
    
    cord.x=c(-5,seq(-5,-abs(z),length.out=1000),-abs(z))
    cord.y=c(0,dnorm(seq(-5,-abs(z),length.out=1000)),0)
    polygon(cord.x,cord.y,col='skyblue')
    
    cord.x=c(abs(z),seq(abs(z),5,length.out=1000),5)
    cord.y=c(0,dnorm(seq(abs(z),5,length.out=1000)),0)
    polygon(cord.x,cord.y,col='skyblue')
    
    text(z,0.2,paste("z=",z)) 
    
    if(2*(1-pnorm(abs(z)))<0.05){
      text(4,0.2,"Reject H0") #reject H0
      }else(text(4,0.2,"Don't Reject H0")) 
    
  })
})
