#### Load packages ----
library(shiny)
library(tidyverse)
library(readxl)

#### Load data ----
SDG_Data <- read_excel("../SDGEXCEL.xlsx")


SDG_Data<- SDG_Data %>%
    filter(`Country Name`=="Brazil"|`Country Name`=="China"|`Country Name`=="United States" | `Country Name`== "Russian Federation" |`Country Name`=="India" | `Country Name` == "Canada" | `Country Name`== "Argentina" | `Country Name`=="Chile" | `Country Name`== "Japan"| `Country Name`== "Germany" | `Country Name`=="Nigeria"| `Country Name`== "Saudi Arabia"| `Country Name`=="Indonesia") %>%
    filter(`Indicator Name`== "CO2 emissions (metric tons per capita)" | `Indicator Name`=="PM2.5 air pollution, mean annual exposure (micrograms per cubic meter)" |`Indicator Name`=="Renewable electricity output (% of total electricity output)"| `Indicator Name`=="Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70, female (%)"|`Indicator Name`=="GDP per capita (current US$)"| `Indicator Name` == "Access to clean fuels and technologies for cooking (% of population)") %>%
    mutate(Year15 = `2015`) %>%
    select(`Country Name`, `Indicator Name`, `Year15`) 

SDG_Data$Year15<-as.numeric(SDG_Data$Year15)

SDG_Data.short<-SDG_Data %>%
    spread( `Indicator Name`, `Year15`)


SDG_Data.short<-SDG_Data.short %>%
    rename(Country = `Country Name`, CO2 = `CO2 emissions (metric tons per capita)`, Mortality = `Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70, female (%)`, GDP = `GDP per capita (current US$)`, PM2.5 = `PM2.5 air pollution, mean annual exposure (micrograms per cubic meter)`, Renew = `Renewable electricity output (% of total electricity output)`, Access = `Access to clean fuels and technologies for cooking (% of population)`)



#### Define UI ----
ui <- fluidPage(
    titlePanel("CO2 emissions versus Mortality Rate"),
    sidebarLayout(
        sidebarPanel(
            
            # Select nutrient to plot
            selectInput(inputId = "y", 
                        label = "Variable",
                        choices = c("CO2"," Mortality",  "GDP" ,  "PM2.5", "Renew",  "Access"), 
                        selected = "CO2")
               ),
     
        
        
        # Output
        mainPanel(
            plotOutput("scatterplotty")
        )))



#### Define server  ----
server <- function(input, output) {
    
    # Create a ggplot object for the type of plot you have defined in the UI  
    output$scatterplotty <- renderPlot({
        ggplot(SDG_Data.short, 
               aes_string(x = input$y, y = "Mortality", color= "Country")) +
            geom_point(alpha = 0.8, size = 2) +
            theme_classic(base_size = 14) +
            scale_shape_manual(values = c(21, 24)) +
            labs(x = input$y, y = "Mortality") +
            scale_fill_distiller(palette = "YlOrBr", guide = "colorbar", direction = 1)
        #scale_fill_viridis_c(option = "viridis", begin = 0, end = 0.8, direction = -1)
    })
    
    
}





#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)

