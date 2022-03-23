library(shiny)
library(shinyjs)
library(shinyBS)

##################
##   Molarity   ##
##################
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

##########################
##   Serial Dilutions   ##
##########################
conc.choices <- list('nM' = 1 ,'μM' = 1000, 'mM' = 1000^2, 'M' = 1000^3)

ui <- fluidPage(
useShinyjs(), #Enables shinyjs
titlePanel(title = 'Pharmacology Calculators', windowTitle = 'Pharmacology Calculators'),
    tabsetPanel(
#####################
##   Molarity UI   ##
#####################
        tabPanel('Molarity',
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
        ),
###################
##   Serial UI   ##
###################
    tabPanel('Serial Dilutions',
            div(style = 'margin: 10px; padding: 10px',
            sidebarLayout(
                ## Sidebar: Calculator Inputs
                sidebarPanel(
                    div(style = 'margin: 10px; padding: 10px',
                    textInput(inputId = 'stock.name', placeholder = 'E.g. cAMP', label = 'Stock solution name'),
                    textInput(inputId = 'diluent.name', placeholder = 'E.g. Water', label = 'Diluent name'),
                    fluidRow(
                        column(8,  
                            numericInput(inputId = 'stock.conc', label = 'Concentration of stock solution', value = '')
                        ),
                        column(4,
                            selectInput(inputId = 'stock.units', label = 'Units', choices = conc.choices, selected = conc.choices[2])
                        )
                    ),
                    fluidRow(
                        column(8,  
                            numericInput(inputId = 'initial.conc', label = 'Serial dilution initial concentration', value = '')
                        ),
                        column(4,
                            selectInput(inputId = 'initial.units', label = 'Units', choices = conc.choices, selected = conc.choices[2])
                        )
                    ),
                    numericInput(inputId = 'dilution.factor', label = 'Dilution factor', value = 3.16),
                    numericInput(inputId = 'final.volume', label = 'Final dilution volume', value = ''),
                    numericInput(inputId = 'n.dilutions', label = 'Number of dilutions', value = ''),
                    div(style = 'text-align: center; margin: 10px;',
                        bsButton(inputId = 'calculate.button', label = 'Generate Protocol', style = 'primary')
                    )
                    )
                ),
                ## Main panel: Calculator outputs are rendered upon clicking action button
                mainPanel(
                    tableOutput('protocol.tb')
                )
            )
            )
        )
)
)

server <- function(input, output, session) {
##################
##   Molarity   ##
##################
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
    
#########################
##  Serial Dilutions   ##
#########################
    ## Disable Calculate Button unless all fields are filled.
    observe({
        inputs <- c(input$stock.name, input$diluent.name, input$stock.conc, input$initial.conc, input$dilution.factor, input$final.volume, input$n.dilutions)
        if(TRUE %in% (is.na(inputs)) || "" %in% inputs) {
            shinyjs::disable("calculate.button")
        } else {
            shinyjs::enable("calculate.button")
        }
    })
    ## Create a table that summarises dilution protocol
    observeEvent(input$calculate.button, {
        ## Define some useful things
        tb.names.str <- paste0('Concentration ', '(', names(conc.choices[which(conc.choices == input$initial.units)]), ')')
        tb.sources.str <- paste0('From ', input$stock.conc, ' ', names(conc.choices[which(conc.choices == input$stock.units)]), ' stock')
        tb.sources <- c(tb.sources.str, 2:input$n.dilutions)
        ## Diluent Volume Column
        tb.diluent <- rep(input$final.volume, input$n.dilutions)
        tb.diluent[1] <- (input$final.volume / (1 - (input$dilution.factor / 10))) # Dilution #1 = ((desired vol / (1 - DF/10)) - drug volume)
        ## Stock Volume Column
        conc.ce <- (as.numeric(input$initial.units) / as.numeric(input$stock.units)) ## Defines concentration coefficient
        tb.drug.vol <- conc.ce * ((input$initial.conc * tb.diluent[1]) / input$stock.conc) ## Volume of drug stock; derived from C1*V1 = C2*V2
        tb.diluent[1] <- tb.diluent[1] - tb.drug.vol ## Correct tb.diluent value as it is calculated from the total volume (i.e. including drug)
        tb.drug <- rep(((tb.diluent[1] - input$final.volume) + tb.drug.vol), input$n.dilutions)
        tb.drug[1] <- tb.drug.vol
        ## Source Column
        i <- 2 # start counter at 2 as pos[1] is defined already
        while (i <= input$n.dilutions) {
            tb.sources[i] <- paste0('From Dilution ', (i - 1))
            i <- i + 1
        }
        ## Concentration column
        tb.concentration <- c(1:input$n.dilutions)
        tb.concentration[1] <- input$initial.conc
        i <- 2
        while (i <= input$n.dilutions) {
            tb.concentration[i] <- (tb.concentration[i-1] / input$dilution.factor)
            i <- i + 1
        }
        ## Create an empty dataframe, name it then fill with data
        protocol.tb <- data.frame(matrix(ncol = 5, nrow = input$n.dilutions))
        names(protocol.tb) <- c('Dilution #', paste0(input$stock.name, ' Volume (uL)'), 'Source', paste0(input$diluent.name, ' Volume (uL)'), tb.names.str)
        protocol.tb[, 1] <- c(1:input$n.dilutions) ## Set dilution column
        protocol.tb[, 2] <- tb.drug ## Set Drug volume column
        protocol.tb[, 3] <- tb.sources ## Set sources column
        protocol.tb[, 4] <- tb.diluent ## Set Diluent volume column
        protocol.tb[, 5] <- formatC(tb.concentration, digits = 5) ## Set Concentration column
        output$protocol.tb <- renderTable({protocol.tb})
    })
}

shinyApp(ui, server)