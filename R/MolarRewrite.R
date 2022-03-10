library(shiny)
library(tableHTML)

## Define Input UI elements
calc.input <- function(value, integer) {
    .placeholdertext <- c('Concentration 1', 'Volume 1', 'Concentration 2', 'Volume 2')[integer]
    div(style = 'margin: auto; width: 85%; padding: 0px',
        textInput(inputId = as.character(value), placeholder = .placeholdertext, width = '100%', label = '', value = '')
    )
}

ui <- fluidPage(
    titlePanel(
            title = 'Dilution Calculator', windowTitle = 'Dilution Calculator'
    ),
    div(style = 'margin: 10px; width: 300px; padding: 10px', 
        sidebarLayout(
            fluid = FALSE,
            sidebarPanel(
                tags$style(make_css(list('.well', 'padding', '0px'), list('.well', 'margin', '0px'))),
                width = 12,
                calc.input('c1', 1),
                calc.input('v1', 2),
                div(style = 'text-align: center; font-size: 38px; font-weight: bold; padding: 0px;',
                    "="
                ),
                calc.input('c2', 3),
                calc.input('v2', 4),
            ),
            mainPanel(
                width = 0
            )
        )
    )
)

server <- function(input, output, session) {
    ## Dilution calculation
    dilution.calc <- reactive({
        function(c1 = '', v1 = '', c2 = '', v2 = '') {
            if (length(which(c(c1, v1, c2, v2) == '')) != 4) {
                c1 <- input$c1 
                v1 <- input$v1 
                c2 <- input$c2
                v2 <- input$v2
                .values <- list(c1, v1, c2, v2)
                .int <- which(.values == '')
                ## positions 1 & 3 have coefficient @ +1; positions 2 & 4 have coefficient @ -1
                if (.int == 1) {.solution <- (c2 * v2)/v1}
                if (.int == 2) {.solution <- (c2 * v2)/c1}
                if (.int == 3) {.solution <- (c1 * v1)/v2}
                if (.int == 4) {.solution <- (c1 * v1)/c2}
            } else {
                .solution <- ''
            }
            .solution
        }
    })
}

shinyApp(ui, server)