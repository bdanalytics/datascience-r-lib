require (shiny)
#runApp(display.mode='showcase')

require(ggplot2)

# before runApp() run the following

# server initilization stuff
#x <<- x + 1
#y <<- 0

shinyServer(
    function(input, output) {
       
        dataset <- reactive({
            diamonds[sample(nrow(diamonds), input$sampleSize), ]
        })
        
        output$plot <- renderPlot({
            p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
            
            if (input$color != 'None')
                p <- p + aes_string(color=input$color)
            
            facets <- paste(input$facet_row, '~', input$facet_col)
            if (facets != '. ~ .')
                p <- p + facet_grid(facets)
            
            if (input$jitter)
                p <- p + geom_jitter()
            if (input$smooth)
                p <- p + geom_smooth()
            
            print(p)
            
        }, height=700)
        
        #output$text3 <- renderText({
        #    #input$goButton
        #    #isolate(react())
        #    
        #         if (input$goButton == 0) "Press Go! button to see results"
        #    else if (input$goButton <= 5) isolate(react())
        #    else "Number of attempts exceeded limit(5)"
        #})
    }
)