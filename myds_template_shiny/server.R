#print("server code being executed...")
#rm(list=ls())
#setwd()

source("config.R")      # Ensure config.R is in .gitignore
source("uisrvrinit.R")

# before runApp() run the following

# debugging options
#runApp(display.mode='showcase')
#cat() -> displays output to stdout (so R console)
#browser() -> can interupt execution and can be called conditionally
#http://shiny.rstudio.com/articles/debugging.html

# to deploy
# authorize computer with token from https://www.shinyapps.io/tokens
#require(shinyapps)
#deployApp()
#terminateApp("<app-name>")

# server initilization stuff
#x <<- x + 1
#py <- plotly(username=plotly_username, key=plotly_key)

shinyServer(
    function(input, output) {
       
#         y <<- y + 1
        
#         react_fn <- reactive({as.numeric(input$text1) + as.numeric(input$text2)})

#         diamonds_smp_df_fn <- reactive({
#             diamonds_df[sample(nrow(diamonds_df), input$plot.sampleSize), ]
#         })

#         test_diamonds_df_fn <- reactive({
#             if (!is.na(input$predict.carat))
#                 test_diamonds_df$carat <- input$predict.carat
#             else
#                 test_diamonds_df$carat <- median_diamonds_df$carat  
#             
#             if (!is.na(input$predict.cut))
#                 test_diamonds_df$cut <- input$predict.cut
#             else
#                 test_diamonds_df$cut <- median_diamonds_df$cut  
#             
#             predict_price(test_diamonds_df) 
#         })

#         create_ggplot_reactive_fn <- reactive({
#             #cat("\nin create_ggplot_reactive_fn:")
#             myplot_scatter (diamonds_smp_df_fn(), input$plot.x, "price",
#                             colorcol_name=input$plot.color,
#                             jitter=input$plot.jitter, smooth=input$plot.smooth,
#                             facet_rowcol_name=input$plot.facet_row,
#                             facet_colcol_name=input$plot.facet_col,
#                             ylabel="price ($)",
#                             stats_df=median_diamonds_df,
#                             predict_df=test_diamonds_df_fn()
#                             , i_pkg="plotly"
#                             )
#         })

#         output$debug_str <- renderText(sprintf("test_diamonds_df:\n%s",
#                                                test_diamonds_df_fn()))
#
        # Sorted by render*
 
#         output$newHist <- renderPlot({
#             hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
#             mu <- input$mu
#             lines(c(mu, mu), c(0, 200),col="red",lwd=5)
#             mse <- mean((galton$child - mu)^2)
#             text(63, 150, paste("mu = ", mu))
#             text(63, 140, paste("MSE = ", round(mse, 2)))
#         }, height=700)
#         output$plot <- renderPlot({
#             gp <- create_ggplot_reactive_fn()
#             print(gp)            
#         }, height=700)
#
#         
#         output$prediction <- renderPrint({diabetesRisk(input$glucose)})
#
#         output$text.y <- renderText(y)
#         output$price_predict_str <- renderText(
#             paste(sprintf("price: $%s; 95%% conf interval: [$%s, $%s]", 
#                     format(test_diamonds_df_fn()$price.predict.fit, big.mark=","),                    
#                     format(test_diamonds_df_fn()$price.predict.lwr, big.mark=","),                    
#                     format(test_diamonds_df_fn()$price.predict.upr, big.mark=","))
#                  ,sprintf("\n for a diamond with specified features: %s", 
# # Error: Single-bracket indexing of reactivevalues object is not allowed.                                
# #                           paste(sapply(c("predict.carat", "predict.cut"), 
# #                                        function(feat) paste0(feat, "=", 
# #                                                              print(input[feat]))), 
# #                                 collapse="; "))
#                             paste(paste0("carat = ", print(input$predict.carat)),
#                                   paste0("cut = ",   print(input$predict.cut)),
#                                   sep="; "))
#                  ,sprintf("\n and other features default to training data medians")
#             )
#         )
#         output$text1 <- renderText({input$text1})
#         output$text3 <- renderText({
#             #input$goButton
#             #isolate(react_fn())
#             
#                  if (input$goButton == 0) "Press Go! button to see results"
#             else if (input$goButton <= 5) isolate(react_fn())
#             else "Number of attempts exceeded limit(5)"
#         })
#       
#         output$iplot <- renderUI({
#             pyout <- py$ggplotly(create_ggplot_reactive_fn())
#             embed_url <- pyout$response$url
#             embed_txt <- paste0(
#                 '<iframe width="800" height="600" frameborder="0" ',
#                 'seamless="seamless" scrolling="no" src="',
#                 embed_url,
#                 '.embed?width=800&height=600"></iframe>'
#             )
# #             cat("\nembed_txt:", embed_txt)
#             HTML(embed_txt)
#         })
#         output$iplot <- renderUI({  # "iplot" to be used as argument in server.R
#             subset_iris <- iris[iris$Species %in% input$check_group, ]  # Subset dataset
#             ggiris <- qplot(x=Petal.Width, y=Sepal.Length, data=subset_iris, color=Species) 
#             py <- plotly(username="r_user_guide", key="mw5isa4yqp")  # Open Plotly connection
#             res <- py$ggplotly(ggiris, kwargs=list(filename="Plotly in Shiny", 
#                                                    fileopt="overwrite", # Overwrite plot in Plotly's website
#                                                    auto_open=FALSE))
#             tags$iframe(src=res$response$url,
#                           frameBorder="0",  # Some aesthetics
#                           height=400,
#                           width=650)
# 
#         })
#         output$iplot <- renderUI({
#             pyout <- py$ggplotly(create_ggplot_reactive_fn(),
#                                  kwargs=list(filename="DiamondsApp: iplot",
#                                              fileopt="overwrite", # Overwrite plot in Plotly's website
#                                              auto_open=FALSE))
#             tags$iframe(src=pyout$response$url,
#                         frameBorder="0",  # Some aesthetics
#                         height=600, width=800)
#         })     
    
    }
)