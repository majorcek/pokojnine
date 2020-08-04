library(shiny)
library(shinyWidgets)
library(shinythemes)
library(readr)
library(knitr)
library(dplyr)
library(purrr)


source("funkcija_pokojnine.R")


ui <- fluidPage(

    titlePanel("Pričakovana pokojnina iz prvega stebra"),

    sidebarLayout(
        sidebarPanel(
            wellPanel(
                fluidRow("VAŠA STAROST"),
                fluidRow(
                    column(6, numericInput(inputId = "starost_leto", label = "leto", min = 15, max = 75, value = 40)),
                    column(6, numericInput(inputId = "starost_mesec", label = "mesec", min = 0, max = 11, value = 0))
                ),
                hr(),
                fluidRow("VAŠA STAROST OB UPOKOJITVI"),
                fluidRow(
                    column(6,numericInput(inputId = "upokojitvena_starost_leto", label = "leto", min = 55, max = 75, value = 65)),
                    column(6,numericInput(inputId = "upokojitvena_starost_mesec", label = "mesec", min = 0, max = 11, value = 0))
                )
            ),
            
            radioButtons(inputId = "spol", label = "spol", choices = c("moški" = "m", "ženska" = "z")),
            
            wellPanel("PRVA ZAPOSLITEV",
                fluidRow(            
                    column(6, numericInput(inputId = "prva_zaposlitev_leto", label = "Leto", min = 1975, max = 2020, value = 2010)),
                    column(6, numericInput(inputId = "prva_zaposlitev_mesec", label = "mesec", min = 1, max = 12, value = 1))
                )
            ),
            
            wellPanel(
                fluidRow("",
                    column(9, numericInputIcon(inputId = "prva_placa", label = "VIŠINA PRVE PLAČE", icon = list("€"), min = 10, max = 1000000, value = 1000))
                ),
                fluidRow(
                    column(9, numericInputIcon(inputId = "trenutna_placa", label = "VIŠINA TRENUTNE PLAČE", icon = list("€"), min = 200, max = 10000, value = 2000))
                )
            ),
            sliderInput(inputId = "sprememba_place", label = "Pričakovan letni padec ali rast plače (%)", min = -20, max = 20, value = 1),
            hr(),
            
            radioButtons(inputId = "natancnost", label = "Kakšno natančnost želite?", choices = c("bolj natančno", "manj natančno"), selected = "manj natančno"),
            
            conditionalPanel(
                condition = "input.natancnost == 'bolj natančno'",
                
                uiOutput("predhodne_place")
            ),
        ),

        mainPanel(
            fluidRow("Izračun",
                     wellPanel("Vaša pokojnina bo znašala približno ", verbatimTextOutput("znesek"), " €.")
            )
        )
    )
)







server <- function(input, output, session) {
    
    col_names_prej <- reactive({
        if(input$prva_zaposlitev < 2020){
              paste0("placa po ", 5 * seq_len(floor((2020 - input$prva_zaposlitev - 1)/5)), " letih dela")
        }
    })
    
    output$predhodne_place <- renderUI({
        if(input$prva_zaposlitev < 2020){
            map(col_names_prej(), ~ numericInput(.x , .x, value = 1000, min = 100, max = 10000000))
        }
    })
    
    dosedanje_place <- reactive({c(input$`placa po 5 letih dela`, input$`placa po 10 letih dela`, input$`placa po 15 letih dela`, input$`placa po 20 letih dela`, 
                                   input$`placa po 25 letih dela`, input$`placa po 30 letih dela`, input$`placa po 35 letih dela`)})
    
    observeEvent(input$prva_zaposlitev, {
        oznaka <- ""
        if (input$prva_zaposlitev >= 2007){
            oznaka <- "Vaša prva plača v EUR"
        } else if (input$prva_zaposlitev >= 1991){
            oznaka <- "Vaša prva plača v SIT"
        } else {
            oznaka <- "Vaša prva plača v DIN"
        }
        updateNumericInput(session, "prva_placa", label = oznaka)
    })

    output$znesek <- eventReactive(c(
                    input$prva_zaposlitev_leto,
                    input$prva_zaposlitev_mesec,
                    input$prva_placa, 
                    input$spol,
                    input$trenutna_placa,
                    input$sprememba_place, 
                    input$starost_leto,
                    input$starost_mesec,
                    input$upokojitvena_starost_leto,
                    input$upokojitvena_starost_mesec,
                    input$`placa po 5 letih dela`,
                    input$`placa po 10 letih dela`,
                    input$`placa po 15 letih dela`,
                    input$`placa po 20 letih dela`,
                    input$`placa po 25 letih dela`,
                    input$`placa po 30 letih dela`,
                    input$`placa po 35 letih dela`, 
                    input$natancnost),
                  
                    {
                        if(input$natancnost == "manj natančno"){
                            izracunaj_pokojnino(input$spol, input$prva_zaposlitev_leto, input$prva_zaposlitev_mesec, input$prva_placa, input$trenutna_placa, 1 + input$sprememba_place / 100, 894.88, 3579.52, input$starost_leto, input$starost_mesec, input$upokojitvena_starost_leto, input$upokojitvena_starost_mesec)
                        } else if (input$prva_zaposlitev >= 2015){
                            izracunaj_pokojnino(input$spol, input$prva_zaposlitev_leto, input$prva_zaposlitev_mesec, input$prva_placa, input$trenutna_placa, 1 + input$sprememba_place / 100, 894.88, 3579.52, input$starost_leto, input$starost_mesec, input$upokojitvena_starost_leto, input$upokojitvena_starost_mesec)
                        } else {
                            izracunaj_pokojnino2(input$spol, input$prva_zaposlitev_leto, input$prva_zaposlitev_mesec, input$prva_placa, input$trenutna_placa, 1 + input$sprememba_place / 100, 894.88, 3579.52, input$starost_leto, input$starost_mesec, input$upokojitvena_starost_leto, input$upokojitvena_starost_mesec, dosedanje_place())
                        }

                  })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
