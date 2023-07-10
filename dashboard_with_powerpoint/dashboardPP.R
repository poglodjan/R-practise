###
# Streszczony projekt korporacyjny sporządzający prezentacje PowerPoint z dashboardu
# (Dane firmy zamienione na dane IRIS)
###

library(shiny)
library(reticulate)
library(tidyverse)
library(officer)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(webshot)
library(htmlwidgets)

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Generator prezentacji"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Generuj prezentację", tabName = "item1"),
        menuItem("Wybierz wykres", tabName = "item2")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "item1",
          fluidRow(
            box(
              width = 6,
              title = "Opcje prezentacji",
              textInput("slides_num", "Poniżej wpisz tekst:", value=""),
              actionButton("generate_btn", "Generuj prezentację")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Wygenerowana prezentacja",
              uiOutput("presentation_output")
            )
          )
        ),
        tabItem(
          tabName = "item2",
          fluidRow(
            box(
              width = 6,
              title = "Wybierz wykres do prezentacji:",
              checkboxGroupInput("wykresy", "Wybierz wykres:",
                                 choices = c("skrzynkowy", "punktowy", "gestosci"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$generate_btn, {
    showModal(modalDialog(
      title = "Informacja",
      "Ładowanie..."
    ))
    
    if (input$wykresy == "skrzynkowy"){
    wykres <- ggplot(data = iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
      geom_boxplot()
    }
    else if (input$wykresy == "punktowy"){
      wykres <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point() +
        labs(title = "Wykres punktowy", x = "Długość działki kielicha", y = "Szerokość działki kielicha")
    }
    else if (input$wykresy == "gestosci"){
    wykres <- ggplot(data = iris, aes(x = Sepal.Length, fill = Species)) +
      geom_density(alpha = 0.5) +
      labs(title = "Wykres gęstości", x = "Długość działki kielicha", y = "Gęstość")
    }
    
    file_path <- "prezentacja.pptx"
    
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
    presentation <- read_pptx()
    
    final_pres <- add_slide(presentation, layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = "Slajd wstęp", location = ph_location_type(type = "title")) %>%
      ph_with(value = paste0("Witam ",input$slides_num, ", na dalszych slajdach są wybrane wykresy"), 
              location = ph_location_type(type="body"))
    
    if (length(input$wykresy == 1)){
    final_pres <- add_slide(presentation) %>%
      ph_with(value = wykres,location = ph_location_type(type = "body"), bg = "transparent") %>%
      ph_with(value = "Twoj wykres:",location = ph_location_type(type = "title"))
    }

    print(final_pres, "prezentacja.pptx")
    system(paste("open",shQuote("prezentacja.pptx")))
    removeModal()
  })
}

shinyApp(ui, server)
webshot::install_phantomjs(force=TRUE)
webshot::webshot("app.html", "app.png", delay = 5, vwidth = 800, vheight = 600)








