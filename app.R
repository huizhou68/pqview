library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

# 1.Define UI for app that draws a histogram
ui <- fluidPage(
  # App title
  titlePanel(""),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    sidebarPanel(
      
      # Input: Slider for type of plots
      selectInput("PlotType", label="Question",
                  choices=list("Quantile to Probability"="Q2P", 
                               "Probability to Quantile"="P2Q",
                               "Quantile to Probability (Two-tailed)"="Q2P Two-tailed", 
                               "Probability to Quantile (Two-tailed)"="P2Q Two-tailed"),
                  selected="Quantile to Probability"),
      
      # Input: Slider for quantiles (critical values)
      sliderInput(inputId="Quantile",
                  label="Quantile",
                  min=-4,
                  max=4,
                  step=0.01,
                  value=1.64),
      
      # Input: Slider for significance alpha
      sliderInput(inputId="Alpha",
                  label="Significance",
                  min=0.001,
                  max=0.999,
                  step=0.01,
                  value=0.05),
      
      # Input: Slider for df
      sliderInput(inputId="DF",
                  label="Degrees of Freedom",
                  min=1,
                  max=200,
                  step=1,
                  value=5),
      
      # Input: Slider for df1 in F test
      sliderInput(inputId="DF1",
                  label="DF1 (F Distribution)",
                  min=1,
                  max=100,
                  step=1,
                  value=4),
      
      # Input: Slider for df2 in F test
      sliderInput(inputId="DF2",
                  label="DF2 (F Distribution)",
                  min=1,
                  max=300,
                  step=1,
                  value=45)
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Normal", plotOutput("distPlot")),
                  tabPanel("Student's t", plotOutput("TdistPlot")),
                  tabPanel("F", plotOutput("FdistPlot")),
                  tabPanel("Chi2", plotOutput("ChidistPlot"))
      )
      
    ) # closing mainPanel
    
  ) # closing sidebarLayout
  
) # closing fluidPage


# 2.Define server logic

server <- function(input, output) {
  
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  # Tab 1: Normal distribution
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  
  output$distPlot <- renderPlot({
    
    x <- rnorm(5000)
    # Randomly draw 5000 numbers from a standard normal distributions
    y <- dnorm(x)
    # Computing the probability density for each value
    mydata <- as_tibble(cbind(x, y))
    # Data set ready
    Type <- input$PlotType
    # The type of plot
    
    if (Type=="Q2P") {
      
      Prob <- pnorm(input$Quantile)
      Quan <- input$Quantile
      
      mydata %>% ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x <= Quan),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(round(min(x)), round(max(x)), ceiling((round(max(x))-round(min(x)))/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=(Quan+min(x))/2, y=dnorm(Quan)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label="Standard Normal Distribution")
      
    } else if (Type=="P2Q") {
      
      Quan <- qnorm(p=1-input$Alpha)
      Prob <- 1-input$Alpha
      mydata %>% ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x <= Quan),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(round(min(x)), round(max(x)), ceiling((round(max(x))-round(min(x)))/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan, y=dnorm(Quan), label=str_c("x=", as.character(round(Quan, 4))), size=7.5, col="#4B0082")+
        # geom_text(x=(Quan+min(x))/2, y=dnorm(Quan)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label="Standard Normal Distribution")
      
    } else if (Type=="Q2P Two-tailed") {
      
      Prob <- abs(pnorm(input$Quantile)-pnorm(-input$Quantile))
      Quan <- input$Quantile
      Quan1 <- min(input$Quantile, -input$Quantile)
      Quan2 <- max(input$Quantile, -input$Quantile)
      
      mydata %>% ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(round(min(x)), round(max(x)), ceiling((round(max(x))-round(min(x)))/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
        geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan1, y=dnorm(Quan1)/3, label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
        geom_text(x=Quan2, y=dnorm(Quan2)*2/3, label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
        geom_text(x=(Quan1+Quan2)/2, y=dnorm(Quan1)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label="Standard Normal Distribution")
      
    } else if (Type=="P2Q Two-tailed") {
      
      Prob <- 1-input$Alpha
      Quan1 <- qnorm(p=input$Alpha/2)
      Quan2 <- qnorm(p=1-input$Alpha/2)
      
      mydata %>% ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(round(min(x)), round(max(x)), ceiling((round(max(x))-round(min(x)))/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
        geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan1, y=dnorm(Quan1)/3, label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
        geom_text(x=Quan2, y=dnorm(Quan2)*2/3, label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
        geom_text(x=(Quan1+Quan2)/2, y=dnorm(Quan1)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label="Standard Normal Distribution")
      
    }
    
  })
  
  
  
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  # Tab 2: Student's t-distribution
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  output$TdistPlot <- renderPlot({
    
    DF <- input$DF
    # Degrees of freedom
    x <- rt(5000, df=DF)
    # Randomly draw 2000 numbers from a t distribution with 3 DFs
    y <- dt(x, df=DF)
    # Computing the probability density for each value
    mydata <- as_tibble(cbind(x, y))
    # Combining x and y to make them a dataset
    Type <- input$PlotType
    # The type of plot
    
    if (Type=="Q2P") {
      
      Quan <- input$Quantile
      Prob <- pt(input$Quantile, df=DF)
      
      mydata %>% 
        ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan), 
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(round(min(x)), round(max(x)), ceiling((round(max(x))-round(min(x)))/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=(Quan+min(x))/2, y=dnorm(Quan)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("Student's t-distribution", " (df=", as.character(DF), ")"))
      
    } else if (Type=="P2Q") {
      
      Quan <- qt(p=1-input$Alpha, df=DF)
      Prob <- 1-input$Alpha
      
      mydata %>% 
        ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan), 
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(round(min(x)), round(max(x)), ceiling((round(max(x))-round(min(x)))/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan, y=dnorm(Quan), label=str_c("x=", as.character(round(Quan, 4))), size=7.5, col="#4B0082")+
        # geom_text(x=(Quan+min(x))/2, y=dnorm(Quan)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("Student's t-distribution", " (df=", as.character(DF), ")"))
      
    } else if (Type=="Q2P Two-tailed") {
      
      Prob <- abs(pt(input$Quantile, df=DF)-pt(-input$Quantile, df=DF))
      Quan <- input$Quantile
      Quan1 <- min(input$Quantile, -input$Quantile)
      Quan2 <- max(input$Quantile, -input$Quantile)
      
      mydata %>% ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(round(min(x)), round(max(x)), ceiling((round(max(x))-round(min(x)))/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
        geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan1, y=dnorm(Quan1)/3, label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
        geom_text(x=Quan2, y=dnorm(Quan2)*2/3, label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
        geom_text(x=(Quan1+Quan2)/2, y=dnorm(Quan1)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("Student's t-distribution", " (df=", as.character(DF), ")"))
      
    } else if (Type=="P2Q Two-tailed") {
      
      Prob <- 1-input$Alpha
      Quan1 <- qt(p=input$Alpha/2, df=DF)
      Quan2 <- qt(p=1-input$Alpha/2, df=DF)
      
      mydata %>% ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(round(min(x)), round(max(x)), ceiling((round(max(x))-round(min(x)))/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
        geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan1, y=dnorm(Quan1)/3, label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
        geom_text(x=Quan2, y=dnorm(Quan2)*2/3, label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
        geom_text(x=(Quan1+Quan2)/2, y=dnorm(Quan1)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("Student's t-distribution", " (df=", as.character(DF), ")"))
    }
    
  })
  
  
  
  
  
  
  
  
  
  
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  # Tab 3: F Distribution
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  output$FdistPlot <- renderPlot({
    
    DF1 <- input$DF1
    DF2 <- input$DF2
    # Degrees of freedom
    
    x <- rf(2000, df1=DF1, df2=DF2)
    y <- df(x, df1=DF1, df2=DF2)
    
    mydata <- as_tibble(cbind(x, y))
    # Combining x and y to make them a dataset
    Type <- input$PlotType
    # The type of plot
    
    if (Type=="Q2P") {
      
      Quan <- input$Quantile
      Prob <- pf(input$Quantile, df1=DF1, df2=DF2)
      
      ycor <- df(Quan, df1=DF1, df2=DF2)
      
      mydata %>%
        ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan/2, y=ycor/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("F Distribution",
                            " (df1=", as.character(DF1), ",",
                            " df2=", as.character(DF2), ")"))
      
    } else if (Type=="P2Q") {
      
      Quan <- qf(p=1-input$Alpha, df1=DF1, df2=DF2)
      Prob <- 1-input$Alpha
      
      ycor <- df(Quan, df1=DF1, df2=DF2)
      
      mydata %>%
        ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan, y=ycor, label=str_c("x=", as.character(round(Quan, 4))), size=7.5, col="#4B0082")+
        # geom_text(x=Quan/2, y=ycor, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("F Distribution",
                            " (df1=", as.character(DF1), ",",
                            " df2=", as.character(DF2), ")"))
      
    } else if (Type=="Q2P Two-tailed") {
      
      Quan <- input$Quantile
      
      Middle.Value <- qf(p=0.5, df1=DF1, df2=DF2)
      
      if (Quan >= Middle.Value) {
        
        Prob <- 1-(1-pf(input$Quantile, df1=DF1, df2=DF2))*2
        Quan.Other <- qf(p=(1-pf(input$Quantile, df1=DF1, df2=DF2)), df1=DF1, df2=DF2)
        Quan1 <- Quan.Other
        Quan2 <- Quan
        
        ycor1 <- df(Quan1, df1=DF1, df2=DF2)
        ycor2 <- df(Quan2, df1=DF1, df2=DF2)
        
        mydata %>% ggplot(aes(x=x, y=y)) +
          geom_line(size=1)+
          theme_bw()+
          geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                      aes(x=x, ymax=y), ymin=0,
                      fill="#75AADB", alpha=1)+
          labs(x="X", y="Density Function f(x)")+
          scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
          theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                plot.title = element_text(size=25, face="bold", hjust=0.5),
                axis.text=element_text(size=rel(2)),
                axis.title = element_text(size=20, face="bold"),
                axis.ticks.length=unit(.25, "cm"))+
          geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
          geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
          geom_text(x=Quan1, y=ycor1, label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
          geom_text(x=Quan2, y=ycor2, label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
          geom_text(x=(Quan1+Quan2)/2, y=(ycor1+ycor2)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
          ggtitle(label=str_c("F Distribution",
                              " (df1=", as.character(DF1), ",",
                              " df2=", as.character(DF2), ")"))
        
      } else {
        
        Prob <- 1-pf(input$Quantile, df1=DF1, df2=DF2)*2
        Quan.Other <- qf(p=(1-pf(input$Quantile, df1=DF1, df2=DF2)), df1=DF1, df2=DF2)
        Quan1 <- Quan
        Quan2 <- Quan.Other
        
        ycor1 <- df(Quan1, df1=DF1, df2=DF2)
        ycor2 <- df(Quan2, df1=DF1, df2=DF2)
        
        mydata %>% ggplot(aes(x=x, y=y)) +
          geom_line(size=1)+
          theme_bw()+
          geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                      aes(x=x, ymax=y), ymin=0,
                      fill="#75AADB", alpha=1)+
          labs(x="X", y="Density Function f(x)")+
          scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
          theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                plot.title = element_text(size=25, face="bold", hjust=0.5),
                axis.text=element_text(size=rel(2)),
                axis.title = element_text(size=20, face="bold"),
                axis.ticks.length=unit(.25, "cm"))+
          geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
          geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
          geom_text(x=Quan1, y=ycor1, 
                    label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
          geom_text(x=Quan2, y=ycor2, 
                    label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
          geom_text(x=(Quan1+Quan2)/2, y=(ycor1+ycor2)/2, 
                    label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
          ggtitle(label=str_c("F Distribution",
                              " (df1=", as.character(DF1), ",",
                              " df2=", as.character(DF2), ")"))
        
      }
      
    } else if (Type=="P2Q Two-tailed") {
      
      Prob <- 1-input$Alpha
      Quan1 <- qf(p=input$Alpha/2, df1=DF1, df2=DF2)
      Quan2 <- qf(p=1-input$Alpha/2, df1=DF1, df2=DF2)
      
      ycor1 <- df(Quan1, df1=DF1, df2=DF2)
      ycor2 <- df(Quan2, df1=DF1, df2=DF2)
      
      mydata %>% ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
        geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan1, y=ycor1, 
                  label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
        geom_text(x=Quan2, y=ycor2, 
                  label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
        geom_text(x=(Quan1+Quan2)/2, y=(ycor1+ycor2)/2, 
                  label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("F Distribution",
                            " (df1=", as.character(DF1), ",",
                            " df2=", as.character(DF2), ")"))
    }
    
  })
  
  
  
  
  
  
  
  
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  # Tab 4: Chi2 Distribution
  # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  
  output$ChidistPlot <- renderPlot({
    
    DF <- input$DF
    # Degrees of freedom
    x <- rchisq(2000, df=DF)
    # Randomly draw 2000 numbers from a t distribution with 3 DFs
    y <- dchisq(x, df=DF)
    # Computing the probability density for each value
    mydata <- as_tibble(cbind(x, y))
    # Combining x and y to make them a dataset
    Type <- input$PlotType
    # The type of plot
    
    if (Type=="Q2P") {
      
      Quan <- input$Quantile
      Prob <- pchisq(input$Quantile, df=DF)
      
      ycor <- dchisq(Quan, df=DF)
      
      mydata %>% 
        ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan), 
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan/2, y=ycor/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("Chi2 Distribution", " (df=", as.character(DF), ")"))
      
    } else if (Type=="P2Q") {
      
      Quan <- qchisq(p=1-input$Alpha, df=DF)
      Prob <- 1-input$Alpha
      
      ycor <- dchisq(Quan, df=DF)
      
      mydata %>% 
        ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan), 
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan, y=ycor, label=str_c("x=", as.character(round(Quan, 4))), size=7.5, col="#4B0082")+
        # geom_text(x=Quan/2, y=ycor/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("Chi2 Distribution", " (df=", as.character(DF), ")"))
      
    } else if (Type=="Q2P Two-tailed") {
      
      Quan <- input$Quantile
      
      Middle.Value <- qchisq(p=0.5, df=DF)
      
      if (Quan >= Middle.Value) {
        
        Prob <- 1-(1-pchisq(input$Quantile, df=DF))*2
        Quan.Other <- qchisq(p=(1-pchisq(input$Quantile, df=DF)), df=DF)
        Quan1 <- min(Quan, Quan.Other)
        Quan2 <- max(Quan, Quan.Other)
        
        ycor1 <- dchisq(Quan1, df=DF)
        ycor2 <- dchisq(Quan2, df=DF)
        
        mydata %>% ggplot(aes(x=x, y=y)) +
          geom_line(size=1)+
          theme_bw()+
          geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                      aes(x=x, ymax=y), ymin=0,
                      fill="#75AADB", alpha=1)+
          labs(x="X", y="Density Function f(x)")+
          scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
          theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                plot.title = element_text(size=25, face="bold", hjust=0.5),
                axis.text=element_text(size=rel(2)),
                axis.title = element_text(size=20, face="bold"),
                axis.ticks.length=unit(.25, "cm"))+
          geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
          geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
          geom_text(x=Quan1, y=ycor1, label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
          geom_text(x=Quan2, y=ycor2, label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
          geom_text(x=(Quan1+Quan2)/2, y=(ycor1+ycor2)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
          ggtitle(label=str_c("Chi2 Distribution", " (df=", as.character(DF), ")"))
        
      } else {
        
        Prob <- 1-pchisq(input$Quantile, df=DF)*2
        Quan.Other <- qchisq(p=(1-pchisq(input$Quantile, df=DF)), df=DF)
        Quan1 <- min(Quan, Quan.Other)
        Quan2 <- max(Quan, Quan.Other)
        
        ycor1 <- dchisq(Quan1, df=DF)
        ycor2 <- dchisq(Quan2, df=DF)
        
        mydata %>% ggplot(aes(x=x, y=y)) +
          geom_line(size=1)+
          theme_bw()+
          geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                      aes(x=x, ymax=y), ymin=0,
                      fill="#75AADB", alpha=1)+
          labs(x="X", y="Density Function f(x)")+
          scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
          theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                plot.title = element_text(size=25, face="bold", hjust=0.5),
                axis.text=element_text(size=rel(2)),
                axis.title = element_text(size=20, face="bold"),
                axis.ticks.length=unit(.25, "cm"))+
          geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
          geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
          geom_text(x=Quan1, y=ycor1, label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
          geom_text(x=Quan2, y=ycor2, label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
          geom_text(x=(Quan1+Quan2)/2, y=(ycor1+ycor2)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
          ggtitle(label=str_c("Chi2 Distribution", " (df=", as.character(DF), ")"))
        
      }
      
    } else if (Type=="P2Q Two-tailed") {
      
      Prob <- 1-input$Alpha
      Quan1 <- qchisq(p=input$Alpha/2, df=DF)
      Quan2 <- qchisq(p=1-input$Alpha/2, df=DF)
      
      ycor1 <- dchisq(Quan1, df=DF)
      ycor2 <- dchisq(Quan2, df=DF)
      
      mydata %>% ggplot(aes(x=x, y=y)) +
        geom_line(size=1)+
        theme_bw()+
        geom_ribbon(data=subset(mydata, x<=Quan2 & x>=Quan1),
                    aes(x=x, ymax=y), ymin=0,
                    fill="#75AADB", alpha=1)+
        labs(x="X", y="Density Function f(x)")+
        scale_x_continuous(breaks=seq(0, round(max(x)), ceiling(max(x)/10)))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.title = element_text(size=25, face="bold", hjust=0.5),
              axis.text=element_text(size=rel(2)),
              axis.title = element_text(size=20, face="bold"),
              axis.ticks.length=unit(.25, "cm"))+
        geom_vline(xintercept=Quan1, linetype="solid", col="red", lwd=1.5)+
        geom_vline(xintercept=Quan2, linetype="solid", col="red", lwd=1.5)+
        geom_text(x=Quan1, y=ycor1, label=str_c("x1=", as.character(round(Quan1, 4))), size=7.5, col="#4B0082")+
        geom_text(x=Quan2, y=ycor2, label=str_c("x2=", as.character(round(Quan2, 4))), size=7.5, col="#4B0082")+
        geom_text(x=(Quan1+Quan2)/2, y=(ycor1+ycor2)/2, label=str_c("p=", as.character(round(Prob, 4))), size=7.5, col="#006400")+
        ggtitle(label=str_c("Chi2 Distribution", " (df=", as.character(DF), ")"))
    }
    
  })
  
  
  
  
  
  
  
}

# 3.Create Shiny app
shinyApp(ui=ui, server=server)