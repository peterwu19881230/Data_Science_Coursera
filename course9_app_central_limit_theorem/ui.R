

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Demonstration of Central Limit Theorem"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      mainPanel(
        h4("From a standard normal distribution")
        ),
      
      
       sliderInput("NoOfSamples",
                   "Number of samples:",
                   min = 100,
                   max = 5000,
                   value = 100)
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  ),
  
  # Sidebar with a slider input for sample size 
  sidebarLayout(
    sidebarPanel(
      mainPanel(
        h4("From a standard normal distribution")
      ),
      sliderInput("SampleSize",
                  "Sample size:",
                  min = 100,
                  max = 1000,
                  value = 100)
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot2")
    )
  ),
  
  # Sidebar with a slider input for sample size 
  sidebarLayout(
    sidebarPanel(
      mainPanel(
        h4("From a uniform distribution")
      ),
      sliderInput("SampleSize_unif",
                  "Sample size:",
                  min = 100,
                  max = 1000,
                  value = 100)
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot3")
    )
  ),
  
  # Sidebar with a slider input for sample size 
  sidebarLayout(
    sidebarPanel(
      mainPanel(
        h4("From a poisson distribution (lambda = 3)")
      ),
      sliderInput("SampleSize_pois",
                  "Sample size:",
                  min = 100,
                  max = 1000,
                  value = 100)
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot4")
    )
  ),
  
  # Sidebar with a slider input for sample size 
  sidebarLayout(
    sidebarPanel(
      mainPanel(
        h4("Do you reject H0 ~N(10,3) ?")
      ),
      sliderInput("sample_size",
                  "Sample size:",
                  min = 100,
                  max = 1000,
                  value = 100),
      sliderInput("sample_mean",
                  "Sample mean:",
                  min = 9,
                  max = 11,
                  value = 9,
                  step=0.001)
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot5")
    )
  )
  
))
