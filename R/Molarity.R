library(shiny)
library(tableHTML)

## Define Input UI elements
calc.input <- function(inputId, integer) {
    .placeholdertext <- c('Concentration 1', 'Volume 1', 'Concentration 2', 'Volume 2')[integer]
    div(style = 'margin: auto; width: 45%; padding: 0px',
        textInput(inputId = as.character(inputId), placeholder = .placeholdertext, width = '100%', label = '')
    )
}

ui <- fluidPage(
    titlePanel(
            title = 'Dilution Calculator', windowTitle = 'Dilution Calculator'
    ),
    div(style = 'margin: 10px; width: 360px; padding: 10px', 
        sidebarLayout(
            fluid = FALSE,
            sidebarPanel(
                tags$style(make_css(list('.well', 'padding', '0px'))),
                width = 12,
                calc.input('i1', 1),
                calc.input('i2', 2),
                div(style = 'text-align: center; font-size: 38px; font-weight: bold; padding: 0px;',
                    "="
                ),
                calc.input('i3', 3),
                calc.input('i4', 4),
                div(style = 'text-align: center; padding: 15px',
                    actionButton('reset', 'Reset')
                ),
            ),
            mainPanel(
                width = 0
            )
        )
    )
)

server <- function(input, output, session) {
    observe({
        inputs <- c(input$i1, input$i2, input$i3, input$i4)
        if (sum(inputs == '') == 1) {
            missing.int <- which(inputs == '')
            if (missing.int == 1) {
                solution <- signif(((as.numeric(input$i3) * as.numeric(input$i4)) / as.numeric(input$i2)), digits = 5)
            }
            if (missing.int == 2) {
                solution <- signif(((as.numeric(input$i3) * as.numeric(input$i4)) / as.numeric(input$i1)), digits = 5)
            }
            if (missing.int == 3) {
                solution <- signif(((as.numeric(input$i1) * as.numeric(input$i2)) / as.numeric(input$i4)), digits = 5)
            }
            if (missing.int == 4) {
                solution <- signif(((as.numeric(input$i1) * as.numeric(input$i2)) / as.numeric(input$i3)), digits = 5)
            }
            updateTextInput(
                inputId = paste0('i', missing.int),
                value = solution
            )
        }
    })
    observeEvent(input$reset, {
        i <- 1
        while (i <= 4) {
            updateTextInput(
                inputId = paste0('i', i),
                value = ''
            )
            i <- i + 1
        }
    })
}

shinyApp(ui, server)