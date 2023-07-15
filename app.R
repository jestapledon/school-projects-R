
library(shiny)
library(rsconnect)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(stringr)
##rsconnect::deployApp('R projects/Lab3')
laptops_test <- read.csv("laptops_test.csv", sep = ",", header = TRUE)
  
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Price of Laptops"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "select", label = h3("Laptop Component"), 
                  choices = list("Weight", "RAM", "Screen.Size"), 
                  selected = "Weight"),
      radioButtons("radio", label = h3("Manufacturer"), 
                         choices = list("HP", "Asus", "Dell", "Toshiba", 
                                        "Lenovo", "Fujitsu", "Acer", "MSI",
                                        "Vero", "Apple", "Razer", "Samsung"),
                         selected = "HP")
    ),
    mainPanel(plotOutput("scatterplot"),
              "The price is listed in INR, however, the dataset that I pulled this
              information from has listed the price in such a way that conversions dont make sense. 
              (it is likely that the last two digits were meant to be decimals)")
    
  )
)

server <- function(input, output) {
  df1 <- laptops_test
  options(scipen=999)
  df1$Weight <- as.double(str_extract(df1$Weight, "^[0-9.]*\\s?"))
  df1$Screen.Size <- as.double(str_extract(df1$Screen.Size, "^[0-9.]*\\s?"))
  df1$RAM <- as.numeric(str_extract(df1$RAM, "^[0-9.]*\\s?"))
  
  output$scatterplot <- renderPlot({ggplot(
    data = filter(df1, df1$Manufacturer == input$radio)) +
    aes_string(x = input$select,
               y = (filter(df1, df1$Manufacturer == input$radio))$Price) +
      geom_point(alpha = 1) +
      geom_smooth(method = 'lm', se = FALSE) +
      theme_light() +
      labs( x = input$select, y = 'Price in INR', title = 'Price of Laptops v. Component')
  })
  ##output$value <- renderPrint({ input$checks })
}
 
shinyApp(ui = ui, server = server)
