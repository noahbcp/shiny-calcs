library(shiny)
library(tableHTML)

## Function to define UI elements.
calc.input <- function(inputId, integer, units) {
    .placeholdertext <- c('Concentration 1', 'Volume 1', 'Concentration 2', 'Volume 2')[integer]
    if (units == 'C') {
        choices <- list('nM' = 1 ,'μM' = 1000, 'mM' = 1000^2, 'M' = 1000^3)
    }
    if (units == 'V') {
        choices <- list('μL' = 1, 'mL' = 1000, 'L' = 1000^2)
    }
    div(style = 'text-align: center',
        div(style = "margin: auto; display: inline-block;vertical-align:top; width: 60%;",
            textInput(inputId = paste0(inputId), placeholder = .placeholdertext, width = '100%', label = '')
        ),
        div(style = "margin: auto; display: inline-block;vertical-align:top; width: 30%;",
            selectInput(inputId = paste0('units.', inputId), label = '', choices = choices, selected = choices[2])
        )
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
                calc.input('i1', 1, 'C'),
                calc.input('i2', 2, 'V'),
                div(style = 'text-align: center; font-size: 38px; font-weight: bold; padding: 0px;',
                    "="
                ),
                calc.input('i3', 3, 'C'),
                calc.input('i4', 4, 'V'),
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
        unit.inputs <- as.numeric(c(input$units.i1, input$units.i2, input$units.i3, input$units.i4))
        ## Defines the coefficients to calculate unit differences.
        conc.ce <- unit.inputs[3] / unit.inputs[1]
        vol.ce <- unit.inputs[4] / unit.inputs[2]
        if (sum(inputs == '') == 1) {
            missing.int <- which(inputs == '')
            if (missing.int == 1) {
                solution <- signif((conc.ce * vol.ce * (as.numeric(input$i3) * as.numeric(input$i4)) / as.numeric(input$i2)), digits = 5)
            }
            if (missing.int == 2) {
                solution <- signif((conc.ce * vol.ce * (as.numeric(input$i3) * as.numeric(input$i4)) / as.numeric(input$i1)), digits = 5)
            }
            if (missing.int == 3) {
                solution <- signif((conc.ce * vol.ce * (as.numeric(input$i1) * as.numeric(input$i2)) / as.numeric(input$i4)), digits = 5)
            }
            if (missing.int == 4) {
                solution <- signif((conc.ce * vol.ce * (as.numeric(input$i1) * as.numeric(input$i2)) / as.numeric(input$i3)), digits = 5)
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