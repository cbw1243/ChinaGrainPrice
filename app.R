#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(shiny)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(    
  # Give the page a title
  #titlePanel("Monthly price of grain commodities in China"),
  sidebarPanel(
    selectInput("commosel", "Commodity:", 
                choices = c('rice', 'wheat', 'maize')),
    # selectInput("currency", "Currency:", 
    #             choices = c('US dollar', 'Chinese yuan')),
        hr()
        #text("Data from AT&T (1961) The World's Telephones.")
  ),
  # Create a spot for the barplot
  mainPanel(
    plotOutput("pricePlot"),
    textOutput("text1"),
    hr(),
    helpText("Suggested citation: Chen, B., N. Villoria, T. Xia (2019). Tariff Quota Administration in China’s Grain Markets: An Empirical Assessment. Agricultural Economics (Forthcoming)")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  currencyData <- fread('Currency_IMF.csv') %>%
    mutate(Date = as.Date(Date, format = '%d-%b-%Y'),
           Year = year(Date), Month = month(Date)) %>%
    group_by(Year, Month) %>%
    summarise(Currency = mean(Currency))
  #input <- list(commosel = 'rice', currency = 'Chinese yuan')
  
  plotData <- reactive({
    outData <-  fread('china_month_price_moa_update.csv') %>%
      mutate(time = as.Date(time, format = '%m/%d/%Y'),
             Year = year(time), Month = month(time),
             price = price*1000)  %>% 
     left_join(., currencyData, by = c('Year',  'Month')) %>%
     mutate(price = price/Currency) %>%
     filter(commo == input$commosel)
    return(outData)
  })
  
  output$text1 <- renderText({
    Notes <- ifelse(input$commosel == 'rice', 
                    paste0('Notes: Domestic price is average wholesale price of No.1 late Indica rice observed at Huangpu port in Southern China. ',
                           'International price is price of Thai white long grain rice (25% broken), after duties and taxes, observed at Huangpu port in Southern China. '),
                    ifelse(input$commosel == 'wheat', 
                           paste0('Notes: Domestic price is price of high quality wheat observed at Huangpu port in Southern China. ',
                                  'International price is price of U.S hard red winter wheat, after duties and taxes, observed at Huangpu port in Southern China. '),
                           paste0('Notes: Domestic price is exit price of No.2 yellow maize shipped from northeastern China observed at Huangpu port in Southern China. ',
                                  'International price is price of U.S. No.2 yellow maiz, after duties and taxes, observed at Huangpu port in Southern China. ')
                    ))
    Notes <- paste0(Notes, 'Original price data are measured in Chinese yuan and are then converted to US dollars using IMF currency data. Data source: Ministry of Agriculture of China.')
    
  })
  # Fill in the spot we created for a plot
  output$pricePlot <- renderPlot({
     
    plotData() %>%
      ggplot(data = . ) +
      geom_line(aes(time, price, col = note, group = note)) +
      scale_color_manual(values = c('red', 'deepskyblue4'),
                         labels = c('Domestic price', 'International price'),
                         name = '') +
      scale_y_continuous(breaks = seq(round(min(plotData()$price), 0),
                                      round(max(plotData()$price), 0),
                                      by = 20)) +
      scale_x_date(date_breaks = '6 months', date_labels = '%Y/%m') +
      labs(x = 'Year / Month', y = 'Price (US dollar per tonne)',
           title = paste0('Figure. Monthly prices of ', input$commosel, ' in China')) +
      theme_light() +
      theme(legend.position = 'bottom',
            plot.title = element_text(hjust = 0.5, face = 'bold', size = 16),
            plot.caption = element_text(hjust = 0),
            axis.text = element_text(color = 'black', size = 11),
            legend.text = element_text(color = 'black', size = 14))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


