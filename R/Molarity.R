library(shiny)

## Styles
in.block.100px <- function(...) {
    div(style="display: inline-block;vertical-align:top; width:100px ;", ...)
}

ui <- fluidPage(
    titlePanel(
        'Dilution Calculator'
    ),
    uiOutput('calc.ui')
)

server <- function(input, output, session) {
    rea.value <- reactive({
        function(pos){
            .inputvalues <- list(input$c1, input$v1, input$c2, input$v2)
            .nullinputvalues <- sapply(sapply(.inputvalues, is.null), as.integer)
            if (sum(.nullinputvalues) == 1) {
                .outputvalues <- list(output$c1a, output$v1a, output$c2a, output$v2a)
                value <- .outputvalues[pos]
                value <- as.character(value)
                value
            } else {
                if (sum(.nullinputvalues) < 4) {
                    value <- .inputvalues[pos]
                    value <- as.character(value)
                    value
                }
            }
        }
    })
    
    ## Reactive UI block
    output$calc.ui <- renderUI({
        mainPanel(width = 12,
        fluidRow(
            in.block.100px(
                textInput(inputId = 'c1', label = '', placeholder = 'Conc. 1', width = '75px', value = rea.value()(1))
            ),
            in.block.100px(
                textInput(inputId = 'v1', label = '', placeholder = 'Vol. 1', width = '75px', value = rea.value()(2))
            ),
            in.block.100px(
                textInput(inputId = 'c2', label = '', placeholder = 'Conc. 2', width = '75px', value = rea.value()(3))
            ),
            in.block.100px(
                textInput(inputId = 'v2', label = '', placeholder = 'Vol. 2', width = '75px', value = rea.value()(4))
            )
        ),
        div(),
        fluidRow(
                in.block.100px(
                    textOutput(outputId = 'c1a')
                ),
                in.block.100px(
                    textOutput(outputId = 'v1a')
                ),
                in.block.100px(
                    textOutput(outputId = 'c2a')
                ),
                in.block.100px(
                    textOutput(outputId = 'v2a')
                )
            )
        )
    })
    
    ## Solve calc block
    calc.dilution <- reactive({
        function(a, b, c) {
            a <- as.numeric(a)
            b <- as.numeric(b)
            c <- as.numeric(c)
            x <- (a * b) / c
            x
        }
    })
    output$c1a <- renderText({
        answer <- as.character(calc.dilution()(input$v2, input$c2, input$v1))
        answer
    })
    output$v1a <- renderText({
        answer <- as.character(calc.dilution()(input$v2, input$c2, input$c1))
        answer
    })
    output$c2a <- renderText({
        answer <- as.character(calc.dilution()(input$v1, input$c1, input$v2))
        answer
    })
    output$v2a <- renderText({
        answer <- as.character(calc.dilution()(input$v1, input$c1, input$c2))
        answer
    })
}

shinyApp(ui, server)