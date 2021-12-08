rm(list = ls())


library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(haven)
library(reactable)
library(ggplot2)
library(purrr)
library(gganimate)
library(gifski)
library(shinyBS)
library(shinyjs)
library(thematic)
library(shinycustomloader)
library(DT)
# setwd("e:/Dropbox/Dropbox/Wealth tax simulator")
# setwd("~/Dropbox/Wealth tax simulator")

range <- 10^(seq(4.5,12,0.1))


listBrackets <- list(
  "100k US$" = 1e5,
  "200k US$" = 2e5,
  "500k US$" = 5e5,
  "1m US$" = 1e6,
  "2m US$" = 2e6,
  "5m US$" = 5e6,
  "10m US$" = 1e7,
  "20m US$" = 2e7,
  "50m US$" = 5e7,
  "100m US$" = 1e8,
  "200m US$" = 2e8,
  "500m US$" = 5e8,
  "1b US$" = 1e9,
  "2b US$" = 2e9,
  "5b US$" = 5e9,
  "10b US$" = 1e10,
  "20b US$" = 2e10,
  "50b US$" = 5e10,                     
  "100b US$" = 1e11)

listBrackets2 <- list(
  "100 - 200k US$" = 1e5,
  "200 - 500k US$" = 2e5,
  "500k - 1m US$" = 5e5,
  "1 - 2m US$" = 1e6,
  "2 - 5m US$" = 2e6,
  "5 - 10m US$" = 5e6,
  "10 - 20m US$" = 1e7,
  "20 - 50m US$" = 2e7,
  "50 - 100m US$" = 5e7,
  "100 - 200m US$" = 1e8,
  "200 - 500m US$" = 2e8,
  "500m - 1b US$" = 5e8,
  "1 - 2b US$" = 1e9,
  "2 - 5b US$" = 2e9,
  "5 - 10b US$" = 5e9,
  "10 - 20b US$" = 1e10,
  "20 - 50b US$" = 2e10,
  "50 - 100b US$" = 5e10,                     
  "100 - 200b US$" = 1e11)


listISO <- list("World" = "WO",
                "Europe" = "QE",
                "North America" = "XB",
                "East Asia" = "QL",
                "Latin America" = "XL",
                "South & South-East Asia" = "XS",
                "Middle-East & North Africa" = "XN",
                "Sub-Saharan Africa" = "XF",
                "Russia & Central Asia" = "XR")
listISO2 <- c(listISO,"All regions" = "WO QE XB QL XL XS XN XF XR")

effrate <- function(x, rates, thresholds) {
  y <- 0 
  if(x > thresholds[[1]]){
    y <- y + (rates[[1]]/100)*(x-thresholds[[1]])/x
  }
  for(i in 2:length(rates)){
    if(x > thresholds[[i]]){
      y <- y + (rates[[i]]/100)*(x-thresholds[[i]])/x + (rates[[i-1]]/100)*(thresholds[[i]]-x)/x
    }
  }
  return(y)
}

ui <- tagList(
  
  navbarPage(
    title = actionLink("main_logo", tagList(
      tags$span("Global wealth tax simulator"))),
    
    tabPanel("About", icon = icon("home"),
             # modalDialog(
             #   title = "Welcome to the WID's global wealth tax simulator",
             #   tags$img(src = "logo-wid-big.png", height="50%", width="50%", align="center"),
             #   footer = modalButton("Continue"),
             #   size = c("m", "s", "l"),
             #   easyClose = FALSE,
             #   fade = TRUE
             # ),
             fluidRow(column(6,offset=3,"Welcome the WID's global wealth tax simulator, a project of the ",tags$a(href="https://inequalitylab.world/en/","World Inequality Lab"),". This simulator is a companion to the World Inequality Report 2022, which details the motives and downfalls of a global wealth tax in its Chapter 7. For more details, see ", tags$a(href="wir2022.wid.world","wir2022.wid.world"), ".",
             br(),
             br(),
             div(img(src = "logo-wid-big.png", height="100px", width="150px"), style="text-align: center;"),
             br(),
             br(),
             "This simulator has been designed so that you can play with parameters and see the outcomes of a wealth tax in various regions, its effect on the shares of wealth of various groups and the revenues it would generate. All figures are indicative and should be interpreted with caution, especially those concerning multimillionaires and up. You may notice the figures are different from the World Inequality Report's tables, which is due to slightly different specifications and methodology to allow for many brackets. In future updates, this simulator should be taken as the prime reference as opposed to the World Inequality report.", 
             br(),
             br(),
             "The next panel gives you an overview of the data used in the simulation, and lets you download it for the different available regions. The last panel hosts the simulation, which allows you to define up to 8 tax brackets, and to study the impact on each given group.",
             br(),
             br(),
             "If you are interested in the construction of the data, you can visit ", tags$a(href="wir2022.wid.world/methodology","wir2022.wid.world/methodology"), ". If you have any questions left, feel free to contact us.",
             br(),
             br(),
             "Media: ",tags$a(href="mailto:olivia.ronsain@psemail.eu","olivia.ronsain@psemail.eu"),
             br(),
             "Methodology: ",tags$a(href="mailto:rowaida.moshrif@psemail.eu","rowaida.moshrif@psemail.eu"), " or ",tags$a(href="mailto:felix.bajard@psemail.eu","felix.bajard@psemail.eu")))
    ),
    tabPanel("Data", icon = icon("database"),
             fluidRow("For more detail on the construction of the variables and on the concepts used, see Bajard, F., Chancel, L., Moshrif, R., Piketty, T., “ ",tags$i("Global wealth inequality on WID.world: estimates and imputations")," ” and Bauluz, L., Blanchet, T., Martínez-Toledano, C., Sodano, A., “ ",tags$i("Estimation of Global Wealth Aggregates in WID.world: Methodology")," ”.", 
                      br(),
                      br(),
                      "You can download here the raw data used in the simulator. Select the data for the region you want to study. Keep in mind this data should be interpreted with caution. We also display below a sample of the data to illustrate its structure."),
             fixedRow(
               column(6,selectInput(inputId = "regiondata", label = "", choices = listISO2, selected = "WO QE XB QL XL XS XN XF XR"),
                      bsPopover("regiondata", title ="",
                                "Choose here the region of the world you want to download.", 
                                placement = "right", options = list(container = "body")))
               ),
             # fluidRow("We display here a sample of the data to illustrate its structure:",
             #          br(),
             #          br()),
             fluidRow(column(6, DT::dataTableOutput("sample_table"))),
             fluidRow(column(3,div(downloadButton('downloadcsv',"Download CSV")))),
             fluidRow(br(),
                      br(),
               tags$ul(
               tags$li(tags$i("iso")," is the variable denoting the region. Refer to the ", tags$a(href="https://wid.world/codes-dictionary/", "WID's codes dictionary")," for correspondance."), 
               tags$li(tags$i("threshold")," and ", tags$i("threshold_cst"), " are the threshold to reach each group, respectively in current USD and 2021 constant USD at Market Exchange Rates (MER)."), 
               tags$li(tags$i("mnninc999i")," and ", tags$i("mhweal999i"), " are respectively the aggregate regional income and household wealth in the current year, expressed in 2021 constant USD at MER"), 
               tags$li(tags$i("w")," and ", tags$i("n"), " are the wealth and the number of all individuals above the corresponding threshold in the current year, wealth being expressed in 2021 constant USD at MER."),
               br(),
               br())),
             # fluidRow("For more detail on the construction of these variables and on the concepts used, see Bauluz, L., Blanchet, T., Martínez-Toledano, C., Sodano, A., “",tags$i("Estimation of Global Wealth Aggregates in WID.world: Methodology"),"” and Bajard, F., Chancel, L., Moshrif, R., Piketty, T., “",tags$i("Global wealth inequality on WID.world: estimates and imputations"),"”.")
    ),
    tabPanel("Simulator", icon = icon("chart-bar"),
           fixedPage(
             fluidRow(
               tags$div(
                 tags$img(src = "globe.png", width="100", align="center", class="img-simulator"),
                 tags$h1("GLOBAL WEALTH TAX SIMULATOR", class="title-simulator"),
                 class="simulator-header"
               )
             ),
             sidebarLayout(
               
               sidebarPanel(
                 
                 fixedRow(
                   tags$div(
                     tags$h3("design your preferred wealth tax", class="panel-title"),
                     class = "panel-heading"
                   ),
                   tags$div(
                     tags$p(
                   "Choose a region and select your preferred tax rates for different levels of wealth. You can choose up to 8 wealth brackets.",
                   class = "panel-body"
                   )
                   )
                 ),
                 br(),  
                 
                 fluidRow(
                   column(6,selectInput(inputId = "region",label = "Region", choices = listISO, selected = "WO"),
                          bsPopover("region", title ="",
                                    "Choose here the region of the world you want to study.", 
                                    placement = "right", options = list(container = "body"))),
                   column(6,selectInput(inputId = "nbBrackets", label = "# of Brackets", choices = seq(1,8,1), selected = 4),
                          bsPopover("nbBrackets", title ="",
                                    "Select here the number of tax brackets you have in mind.", 
                                    placement = "right", options = list(container = "body")))),
                 fluidRow(
                   column(6,sliderInput(inputId ="depreciation",  
                                        label = "Depreciation (%)", 
                                        min = 0, max = 100, value = 10),
                          bsPopover("depreciation", title ="",
                                    "Select the value of capital stock depreciation you think is most realistic. The stock depreciation parameter helps to anticipate the potentially negative or positive impacts of wealth taxation on asset prices. Assuming a depreciation parameter of 15% amounts to assuming that the market value of financial and non-financial assets declines by 15% following the introduction of a wealth tax.", 
                                    placement = "right", options = list(container = "body"))),
                   column(6,sliderInput(inputId ="evasion",  
                                        label = "Tax evasion (%)", 
                                        min = 0, max = 100, value = 20),
                          bsPopover("evasion", title ="",
                                    "Select the value of tax evasion you think is most realistic. tax evasion parameter defines the expected share of unreported taxable wealth due to the multiple forms of tax evasion (underreporting, offshoring, fraud, etc.). An evasion rate of 10% means that 10% of the net value of taxable wealth will not be reported and therefore that revenue will be 10% lower than what it could be, absent tax evasion.", 
                                    placement = "right", options = list(container = "body")))),
                 fluidRow(column(5, uiOutput("input_rates_ui"),
                                 bsPopover("input_rates_ui", title ="",
                                           "Select here the tax rate that will apply to all wealth above the corresponding threshold.", 
                                           placement = "right", options = list(container = "body"))),
                          column(7, uiOutput("input_thresh_ui"),
                                 bsPopover("input_thresh_ui", title ="",
                                           "Select here the thresholds. They must be strictly increasing for the simulation to work.", 
                                           placement = "right", options = list(container = "body"))))
               ),
               mainPanel(
                 
                 tabsetPanel(
                   tabPanel("Main results",
                            fixedRow(
                              br(),
                              "How much revenue does your tax generate? How are wealth owners impacted by the tax?",
                              br(),
                              br()
                            ),
                            fluidRow(column(12, tableOutput("struct_table"))),
                            fixedRow(
                              br(),
                              "The revenue column shows how much revenue the tax will generate (as a percentage of regional income) in the corresponding bracket, while the effective tax rate column shows the average amount of tax individuals from the bracket will pay as a percentage of their initial wealth. Effective tax rates are lower than your chosen tax rates since only the top of individuals' wealth is taxed at the highest rate.",
                              br(),
                              br(),
                              "The first row displays the results for all brackets."
                            )),
                            
                   
                   id = "plot_tabs",
                   tabPanel("Effective tax rates",
                            fixedRow(
                              br(),
                              "How would an individual's wealth be impacted by your tax?",
                              br(),
                              br(),
                            ),
                            fluidRow(column(10, offset=1, plotOutput("eff_rate"))),
                            fixedRow(
                              br(),
                              "This graph plots the effective tax rate (the total amount of taxes paid as a percentage of their wealth) for any given wealth, before depreciation and evasion.",
                              br(),
                              br(),
                            )),
                   tabPanel("Tax scenario comparison",
                            fixedRow(
                              br(),
                              "What would be the share of wealth of each group today, had your tax been implemented in 2010?",
                              br(),
                              br()
                            ),
                            fluidRow(column(4, selectInput("t_comparison", label = "Threshold", choices  = listBrackets, selected = 1e9)),
                                     column(4, offset=2, selectInput("t_comparison2", label = "Bracket", choices  = listBrackets2, selected = 1e9))),
                            fluidRow(column(6, plotOutput("comparison")),
                                     column(6,plotOutput("comparison2"))),
                            fixedRow(br(),
                                     "These graphs compare the top share of wealth detained by individuals above a chosen threshold and the share of wealth of a given bracket if the tax had been implemented since 2010 to their actual levels. The top share of wealth is the ratio of the wealth of all individuals beyond the threshold over the total of wealth in the region. The share of wealth is the ratio of the wealth of all individuals included in the bracket over the total of wealth in the region.",
                                     br(),
                                     br(),
                                     "Typically, with progressive tax rates, the wealth share of lower brackets will increase, while the top wealth share will systematically decrease.",
                            ))
                   
                 )
               )
             ),class="tabset"
            )
    ),
    id = "main_navbar",
    selected = "Simulator",
    position = "static-top",
    inverse = TRUE,
    windowTitle = "WID - Global wealth tax simulator",
    theme = "style.css"
  )
)

