#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Workout 02"),
   
   # Sidebar with a slider input for initial amount, annual contribution
   fluidRow(width = 12,
     
     column(width = 4,
            sliderInput("amount",
              label = "initial amount:",
              min = 0,
              max = 100000,
              step = 500,
              value = 1000
            ),
            sliderInput('contrib',
              label = 'Annual Contribution:',
              min = 0,
              max = 50000,
              step = 500,
              value = 2000
            )
            ),
     
     column(width = 4,
            sliderInput("rate",
                        label = "return rate (in %):",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 5
            ),
            sliderInput('growth',
                        label = 'growth rate (in %):',
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 2
            )
     ),
     column(width = 4,
            sliderInput("years",
                        label = "Year:",
                        min = 0,
                        max = 50,
                        step = 1,
                        value = 20
            ),
            selectInput('facet',
                        label = 'Facet:',choices = list('Yes' = TRUE,'No' = FALSE),selected = FALSE
            )
     ),
   
      
      # Show a plot of the generated distribution
      mainPanel(width = 12,
         titlePanel('Timeline'),
         plotOutput("distPlot"),
         titlePanel('Balances'),
         verbatimTextOutput('view')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     future_value <- function(amount, rate, years) {
       return(amount*(1 + rate)^years)
     }
     
     annuity <- function(contrib, rate, years) {
       return(contrib*(((1+rate)^years)-1)/(rate))
     }
     
     growing_annuity <- function(contrib, rate, growth, years) {
       return(contrib*(((1+rate)^years)-(1+growth)^years)/(rate-growth))
     }
     
     
     ### MODE 1: 
     no_contrib <- rep(0, input$years)
     for(i in 0:input$years){
       value <- future_value(amount = input$amount, rate = (input$rate)*0.01, years = i)
       no_contrib[i+1] <- value
     }
     ### MODE 2: 
     fixed_contrib <- rep(0, input$years)
     for(i in 0:input$years){
       value <- future_value(amount = input$amount, rate = (input$rate)*0.01, years = i) + annuity(contrib = input$contrib, rate = (input$rate)*0.01, years = i)
       fixed_contrib[i+1] <- value
     }
     ### MODE 3: 
     growing_contrib <- rep(0, input$years)
     for(i in 0:input$years){
       value <- future_value(amount = input$amount, rate = (input$rate)*0.01, years = i) + growing_annuity(contrib = input$contrib, rate = (input$rate)*0.01, growth = (input$growth)*0.01, years = i)
       growing_contrib[i+1] <- value
     }
     
     modalities <- data.frame('year' = 0:input$years, 'no_contrib' = no_contrib, 'fixed_contrib' = fixed_contrib, 'growing_contrib' = growing_contrib)
     
     if (input$facet){
     all_types <- c(modalities$no_contrib, modalities$fixed_contrib, modalities$growing_contrib)
     types <- c(rep('no_contrib', input$years + 1), rep('fixed_contrib',input$years + 1), rep('growing_contrib', input$years + 1))
     
     df <- data.frame('year' = rep(0:input$years, 3), 'values' = all_types, 'type' = types, 'colors' = rep(c('red', 'blue','yellow'), each = input$years + 1))
     
     
     ggplot(data = df, aes(year, values, group = type, col = colors)) + geom_line() +facet_grid(. ~ type) +
       geom_area(fill = df$colors, alpha = 0.2) + geom_point()+
       scale_color_discrete(name = 'Modality', labels = c('no_contrib', 'fixed_contrib', 'growing_contrib')) +
       xlab('year') + ylab('balance')+ggtitle('Annual Balance for each Savings Modality') + theme_bw()
       
       
     } else {
       all_types <- c(modalities$no_contrib, modalities$fixed_contrib, modalities$growing_contrib)
       types <- c(rep('no_contrib', input$years + 1), rep('fixed_contrib',input$years + 1), rep('growing_contrib', input$years + 1))
       
       df <- data.frame('year' = rep(0:input$years, 3), 'values' = all_types, 'type' = types, 'colors' = rep(c('red', 'blue','yellow'), each = input$years + 1))
       

       ggplot(data = df, aes(year, values, group = type, col = colors)) + geom_line() + geom_point()+
         scale_color_discrete(name = 'Modality', labels = c('no_contrib', 'fixed_contrib', 'growing_contrib')) +
         xlab('year') + ylab('balance')+ggtitle('Annual Balance for each Savings Modality') + theme_bw()

     }
     
   })
   
   output$view <- renderPrint({
     
     
     future_value <- function(amount, rate, years) {
       return(amount*(1 + rate)^years)
     }
     
     annuity <- function(contrib, rate, years) {
       return(contrib*(((1+rate)^years)-1)/(rate))
     }
     
     growing_annuity <- function(contrib, rate, growth, years) {
       return(contrib*(((1+rate)^years)-(1+growth)^years)/(rate-growth))
     }
     
     
     ### MODE 1: 
     no_contrib <- rep(0, input$years)
     for(i in 0:input$years){
       value <- future_value(amount = input$amount, rate = (input$rate)*0.01, years = i)
       no_contrib[i+1] <- value
     }
     ### MODE 2: 
     fixed_contrib <- rep(0, input$years)
     for(i in 0:input$years){
       value <- future_value(amount = input$amount, rate = (input$rate)*0.01, years = i) + annuity(contrib = input$contrib, rate = (input$rate)*0.01, years = i)
       fixed_contrib[i+1] <- value
     }
     ### MODE 3: 
     growing_contrib <- rep(0, input$years)
     for(i in 0:input$years){
       value <- future_value(amount = input$amount, rate = (input$rate)*0.01, years = i) + growing_annuity(contrib = input$contrib, rate = (input$rate)*0.01, growth = (input$growth)*0.01, years = i)
       growing_contrib[i+1] <- value
     }
     
     modalities <- data.frame('year' = 0:input$years, 'no_contrib' = no_contrib, 'fixed_contrib' = fixed_contrib, 'growing_contrib' = growing_contrib)
     
     modalities
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

