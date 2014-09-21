require(shiny)

shinyUI(pageWithSidebar(
    
    headerPanel("myappname: mytitle"),
    
    sidebarPanel(
        h1('H1 text'),
        h2('H2 Text'),
        h3('H3 Text'),
        h4('H4 Text'),

        checkboxGroupInput("id2", "Checkbox",
                           c("Value 1" = "1",
                             "Value 2" = "2",
                             "Value 3" = "3")),

        dateInput("date", "Date:"),
        
        numericInput('glucose', 'Glucose mg/dl', 90, min = 50, max = 200, step = 5),        

        selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),

        sliderInput('sampleSize', 'Sample Size', 
                     min=1, max=nrow(dataset), step=500, 
                     value=min(1000, nrow(dataset)), round=0
                    ),

        textInput(inputId="text1", label="Enter Text1"),
        
        h5(" ")#, submitButton('Submit')
        h5(" ")#, actionButton("goButton", "Go!")
    ),
    
    mainPanel(
        h3('Prediction Results'),
        
        p('text1 + text2:'), textOutput('text3'),
        
        plotOutput('newHist'),
        
        h4('You entered'), verbatimTextOutput("inputValue"),
        
        code('some code'),
        
        p('some ordinary text'),
        
        h5(' ')	# Placeholder so that all other commands have a comma
    )
    
))
