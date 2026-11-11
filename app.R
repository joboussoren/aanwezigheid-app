# Fix voor Chrome download-bug in Shinylive
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

library(shiny)
library(readxl)
library(dplyr)
library(writexl)

ui <- fluidPage(
  titlePanel("Aanwezigheid en cijfers: filter"),
  sidebarLayout(
    sidebarPanel(
      textInput("vaknaam", "Vaknaam", placeholder = "bijv. Strafrecht"),
      numericInput("norm", "Gewenste aanwezigheid (minimaal aantal lessen)",
                   value = 7, min = 0, step = 1),
      fileInput("file_aa", "Excel uit AA", accept = c(".xlsx", ".xls")),
      fileInput("file_grades", "Excel met grades", accept = c(".xlsx", ".xls")),
      downloadButton("download", "Download resultaat")
    ),
    mainPanel(
      h4("Samenvatting"),
      verbatimTextOutput("samenvatting"),
      h4("Studenten in AA maar niet in grades"),
      tableOutput("alleen_aa"),
      h4("Studenten in grades maar niet in AA"),
      tableOutput("alleen_grades"),
      h4("Resultaat"),
      tableOutput("resultaat")
    )
  )
)

server <- function(input, output, session) {
  
  import_AA <- reactive({
    req(input$file_aa)
    read_excel(input$file_aa$datapath) %>%
      select(Studentnummer, Aanwezig, `Aanwezig (buiten rooster)`, Groep) %>%
      filter(!is.na(Studentnummer)) %>%
      mutate(Studentnummer = as.character(Studentnummer))
  })
  
  import_grades <- reactive({
    req(input$file_grades)
    read_excel(input$file_grades$datapath) %>%
      select(`Student ID`, `Final grade`) %>%
      mutate(`Student ID` = as.character(`Student ID`))
  })
  
  alleen_AA <- reactive({
    anti_join(import_AA(), import_grades(),
              by = c("Studentnummer" = "Student ID"))
  })
  
  alleen_grades <- reactive({
    anti_join(import_grades(), import_AA(),
              by = c("Student ID" = "Studentnummer"))
  })
  
  combined <- reactive({
    import_AA() %>%
      full_join(import_grades(), by = c("Studentnummer" = "Student ID")) %>%
      mutate(totale_aanwezigheid = rowSums(
        across(c(Aanwezig, `Aanwezig (buiten rooster)`)), na.rm = TRUE))
  })
  
  resultaat <- reactive({
    req(input$norm)
    combined() %>%
      filter(!grepl("X", Groep, ignore.case = TRUE)) %>%
      filter(
        (`Final grade` == "NAP" |
           suppressWarnings(as.numeric(`Final grade`)) < 5.5) &
          totale_aanwezigheid < input$norm
      )
  })
  
  output$samenvatting <- renderText({
    paste0("Rijen in AA: ", nrow(import_AA()), "\n",
           "Rijen in grades: ", nrow(import_grades()), "\n",
           "Rijen na combineren: ", nrow(combined()), "\n",
           "Rijen in resultaat: ", nrow(resultaat()))
  })
  
  output$download <- downloadHandler(
    filename = function() {
      naam <- trimws(input$vaknaam)
      if (naam == "") naam <- "Vak"
      naam <- gsub("[/\\\\:*?\"<>|]", "-", naam)
      paste0("R_OUTPUT - ", naam, ".csv")
    },
    content = function(file) {
      write.csv(resultaat(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)