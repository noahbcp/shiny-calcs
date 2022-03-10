library(shiny)

## Styles
in.block.100px <- function(...) {
    div(style="display: inline-block;vertical-align:top; width:100px ;", ...)
}

ui <- fluidPage(
    titlePanel(
        'Dilution Calculator'
    ),
    mainPanel(width = 12,
    ## Number inputs
        fluidRow(
        in.block.100px(
            textInput(inputId = 'c1', label = '', placeholder = 'Conc. 1', width = '75px')
            ),
        in.block.100px(
            textInput(inputId = 'v1', label = '', placeholder = 'Vol. 1', width = '75px')
            ),
        in.block.100px(
            textInput(inputId = 'c2', label = '', placeholder = 'Conc. 2', width = '75px')
            ),
        in.block.100px(
            textInput(inputId = 'v2', label = '', placeholder = 'Vol. 2', width = '75px')
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
)

server <- function(input, output, session) {
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