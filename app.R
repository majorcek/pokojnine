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
                fluidRow("VAŠA TRENUTNA STAROST"),
                fluidRow(
                    column(6, numericInput(inputId = "starost_leto", label = "leto", min = 15, max = 80, value = 61)),
                    column(6, numericInput(inputId = "starost_mesec", label = "mesec", min = 0, max = 11, value = 5))
                ),
                
                hr(),
                
                fluidRow("DATUM NASTANKA IZRAČUNA"),
                fluidRow(
                    column(6,numericInput(inputId = "izracun_leto", label = "leto", min = 2000, max = 2020, value = 2019)),
                    column(6,numericInput(inputId = "izracun_mesec", label = "mesec", min = 1, max = 12, value = 1))
                ),
                
                hr(),
                
                fluidRow("DATUM UPOKOJITVE"),
                fluidRow(
                    column(6,numericInput(inputId = "upokojitev_leto", label = "leto", min = 2010, max = 2050, value = 2019)),
                    column(6,numericInput(inputId = "upokojitev_mesec", label = "mesec", min = 1, max = 12, value = 3))
                )
            ),
            
            radioButtons(inputId = "spol", label = "SPOL", choices = c("moški" = "m", "ženska" = "z")),
            
            wellPanel("PRVA ZAPOSLITEV",
                fluidRow(            
                    column(6, numericInput(inputId = "prva_zaposlitev_leto", label = "Leto", min = 1975, max = 2020, value = 1979)),
                    column(6, numericInput(inputId = "prva_zaposlitev_mesec", label = "mesec", min = 1, max = 12, value = 1))
                )
            ),
            
            wellPanel(
                fluidRow("",
                    column(9, numericInputIcon(inputId = "prva_placa", label = "VIŠINA PRVE PLAČE (bruto)", icon = icon("fas fa-money-bill-alt", lib = "font-awesome"), min = 10, max = 10000000, value = 5710))
                ),
                fluidRow(
                    column(9, numericInputIcon(inputId = "trenutna_placa", label = "VIŠINA PLAČE V ČASU IZRAČUNA (bruto)", icon = list("€"), min = 200, max = 10000, value = 812))
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
        if(input$izracun_leto * 12 + input$izracun_mesec - input$prva_zaposlitev_leto * 12 - input$prva_zaposlitev_mesec > 60){
            paste0("placa po ", 5 * seq_len(floor((input$izracun_leto * 12 + input$izracun_mesec - input$prva_zaposlitev_leto * 12 - input$prva_zaposlitev_mesec - 1 )/60)), " letih dela (bruto)")
        }
    })
    
    output$predhodne_place <- renderUI({
        if(input$izracun_leto * 12 + input$izracun_mesec - input$prva_zaposlitev_leto * 12 - input$prva_zaposlitev_mesec > 60){
            map(col_names_prej(), ~ numericInput(.x , .x, value = 100000, min = 100, max = 10000000))
        }
    })
    
    dosedanje_place <- reactive({c(input$`placa po 5 letih dela (bruto)`, input$`placa po 10 letih dela (bruto)`, input$`placa po 15 letih dela (bruto)`, input$`placa po 20 letih dela (bruto)`, 
                                   input$`placa po 25 letih dela (bruto)`, input$`placa po 30 letih dela (bruto)`, input$`placa po 35 letih dela (bruto)`)})


    output$znesek <- eventReactive(c(
                    input$prva_zaposlitev_leto,
                    input$prva_zaposlitev_mesec,
                    input$prva_placa, 
                    input$spol,
                    input$trenutna_placa,
                    input$sprememba_place,
                    input$upokojitev_leto,
                    input$upokojitev_mesec,
                    input$starost_leto,
                    input$starost_mesec,
                    input$upokojitvena_starost_leto,
                    input$upokojitvena_starost_mesec,
                    input$`placa po 5 letih dela (bruto)`,
                    input$`placa po 10 letih dela (bruto)`,
                    input$`placa po 15 letih dela (bruto)`,
                    input$`placa po 20 letih dela (bruto)`,
                    input$`placa po 25 letih dela (bruto)`,
                    input$`placa po 30 letih dela (bruto)`,
                    input$`placa po 35 letih dela (bruto)`, 
                    input$natancnost),
                  
                    {
                        if(input$natancnost == "manj natančno"){
                            izracunaj_pokojnino(input$spol, input$prva_zaposlitev_leto, input$prva_zaposlitev_mesec, input$prva_placa, input$trenutna_placa, 1 + input$sprememba_place / 100, input$starost_leto, input$starost_mesec, input$izracun_leto, input$izracun_mesec, input$upokojitev_leto, input$upokojitev_mesec)
                        } else if (input$prva_zaposlitev_leto * 12 + input$prva_zaposlitev_mesec >= input$izracun_leto * 12 + input$izracun_mesec){
                            izracunaj_pokojnino(input$spol, input$prva_zaposlitev_leto, input$prva_zaposlitev_mesec, input$prva_placa, input$trenutna_placa, 1 + input$sprememba_place / 100, input$starost_leto, input$starost_mesec, input$izracun_leto, input$izracun_mesec, input$upokojitev_leto, input$upokojitev_mesec)
                        } else {
                            a <- floor((input$izracun_leto * 12 + input$izracun_mesec - input$prva_zaposlitev_leto * 12 - input$prva_zaposlitev_mesec - 1 )/60)
                            izracunaj_pokojnino2(input$spol, input$prva_zaposlitev_leto, input$prva_zaposlitev_mesec, input$prva_placa, input$trenutna_placa, 1 + input$sprememba_place / 100, input$starost_leto, input$starost_mesec, input$izracun_leto, input$izracun_mesec, input$upokojitev_leto, input$upokojitev_mesec, dosedanje_place()[1:a])
                        }

                  })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
