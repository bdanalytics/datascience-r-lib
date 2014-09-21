#print("server code being executed...")
#rm(list=ls())
#setwd()

source("uisrvrinit.R")

# before runApp() run the following

#runApp(display.mode='showcase')

# debugging options
#runApp(display.mode='showcase')
#cat() -> displays output to stdout (so R console)
#browser() -> can interupt execution and can be called conditionally
#http://shiny.rstudio.com/articles/debugging.html

# to deploy
# authorize computer with token from https://www.shinyapps.io/tokens
#require(shinyapps)
#deployApp()

# server initilization stuff
#x <<- x + 1


shinyServer(
    function(input, output) {
       
        #y <<- y + 1
        react <- reactive({as.numeric(input$text1) + as.numeric(input$text2)})
        dataset <- reactive({
            diamonds[sample(nrow(diamonds), input$sampleSize), ]
        })

        output$text1 <- renderText({input$text1})
        output$text3 <- renderText({
            #input$goButton
            #isolate(react())
            
                 if (input$goButton == 0) "Press Go! button to see results"
            else if (input$goButton <= 5) isolate(react())
            else "Number of attempts exceeded limit(5)"
        })
        output$text.y <- renderText(y)
        
        #output$newHist <- renderPlot({
        #    hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
        #    mu <- input$mu
        #    lines(c(mu, mu), c(0, 200),col="red",lwd=5)
        #    mse <- mean((galton$child - mu)^2)
        #    text(63, 150, paste("mu = ", mu))
        #    text(63, 140, paste("MSE = ", round(mse, 2)))
        #}, height=700)
        
        #output$prediction <- renderPrint({diabetesRisk(input$glucose)})
    
    }
)