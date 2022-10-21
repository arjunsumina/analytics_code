#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Concept of Residual Plots and COD in Linear Regression"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("variables",
                        "Number of variables:",
                        min = 1,
                        max = 6,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

###################################################################
#It contains 3051 rows and 9 columns. Data set starts from 2018 Jan 
#to 2020 Feb at week level for 113 weeks. It has 8 media channel 
#spend information such as Facebook, Google search Impressions,
#Email Impressions, Youtube ( Paid & Organic ), Affiliate channel
#Views & Overall views. Also, it contains sales information against 
#those spends
###################################################################
media_spend_data <- read.csv("media_spend_data.csv")
names(media_spend_data)
###################################################################
# organic views or paid views have more influence on sales
###################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- total_sales
    output$distPlot <- renderPlot({
      y <- media_spend_data$Sales
      x1 <- media_spend_data$Paid_Views
      x2 <- media_spend_data$Organic_Views
      x3 <- media_spend_data$Google_Impressions
      x4 <- media_spend_data$Email_Impressions
      x5 <- media_spend_data$Facebook_Impressions
      x6 <- media_spend_data$Affiliate_Impressions
      if (input$variables ==6)
        model <- lm(y~x1+x2+x3+x4+x5+x6)
      else if (input$variables ==5)
        model <- lm(y~x1+x2+x3+x4+x5)
      else if (input$variables ==4)
        model <- lm(y~x1+x2+x3+x4)
      else if (input$variables ==3)
        model <- lm(y~x1+x2+x3)      
      else if (input$variables ==2)
        model <- lm(y~x1+x2)     
      else 
        model <- lm(y~x1)
      summary(model)
      res <- resid(model) # get list of residuals
      #produce residual vs. fitted plot
      plot(fitted(model), res,main="Residual Plot",xlab="Predicted yhat",ylab="Residual e=yhat-y")
      r2 <- round(summary(model)[8][[1]],4)
      text(1000000,1.5e6,paste0("COD R2=",r2 ))
      #     cex=.8, pos=3,col="black") 
      #add a horizontal line at 0 
      #abline(0,0)
      #qqplot
      #qqnorm(res)
      #density
      #plot(density(res),main = "Distribution Function of Error")
      
    })
}


# Run the application 
shinyApp(ui = ui, server = server)



