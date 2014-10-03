#print("ui code being executed...")
source("uisrvrinit.R")

shinyUI(pageWithSidebar(
    
#     headerPanel("myappname: mytitle"),
    headerPanel(title=HTML("DiamondsApp:Plotly"),
                windowTitle="Diamonds Price Predictor"),
    
    sidebarPanel(
        h1('H1 text'),
        h2('H2 Text'),
        h3('H3 Text'),
        h4('H4 Text'),

        # Sorted by *Input
        checkboxInput('plot.jitter', 'Jitter'),

        checkboxGroupInput(inputId="check_group",  # ID to be used in server.R
                           label="Select species:",
                           choices=list("Setosa"="setosa",  # Make sure not to mix names with values
                                        "Versicolor"="versicolor",
                                        "Virginica"="virginica"),
                           selected=list("setosa", "versicolor", "virginica")),
                     
        dateInput("date", "Date:"),
        
        numericInput('glucose', 'Glucose mg/dl', 90, min=50, max=200, step=5),        

        selectInput('predict.cut', 'cut', levels(diamonds_df$cut),
                    selected=median_diamonds_df$cut, multiple=FALSE),

        sliderInput('sampleSize', 'Sample Size', 
                     min=1, max=nrow(dataset), step=500, 
                     value=min(1000, nrow(dataset)), round=0
                    ),

        textInput(inputId="text1", label="Enter Text1"),
        
        h5('Notes:'),
        helpText("1. While the plot shows only the ",
                 "specified number of observations,",
                 "the prediction is based on the ",
                 "full dataset."),

        h5(" ")#, submitButton('Submit')
        h5(" ")#, actionButton("goButton", "Go!")
    ),
    
    mainPanel(
        textOutput('debug_str'),
            
        h3('Prediction Results'),

        code('some code'),
        
        p('some ordinary text'),

        tabsetPanel(
            tabPanel("ggplot",
                     plotOutput('plot'),

                     h5(' ')    # Placeholder so that all other commands have a comma
            ),

            tabPanel("plotly",
                     uiOutput('iplot'),

                     h5(' ')    # Placeholder so that all other commands have a comma
            ),

        id="plot.tab"),

        # Sorted by *Output
        htmlOutput("plot")  # Argument name from server.R
                        
        plotOutput('newHist'),
        
        p('text1 + text2:'), textOutput('text3'),
        
        h4('Data Plot:'), uiOutput('iplot'),
        
        h4('You entered'), verbatimTextOutput("inputValue"),
        
        h5(' ')	# Placeholder so that all other commands have a comma
    )
    
))
