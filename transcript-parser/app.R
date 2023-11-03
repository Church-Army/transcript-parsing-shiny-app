library(conflicted)
library(shiny)
library(fs)
library(purrr)
library(stringr)
library(tidyr)
library(dplyr)
conflicted::conflicts_prefer(dplyr::lag)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Parse Zoom transcriptions"),

  fileInput("trans_file",
            "Upload transcript",
            multiple = FALSE,
            accept = c("text/csv", ".txt", ".vtt")
            ),
  textOutput("names"),
  downloadButton("download_trans", "Download tidy transcripts")
)



# Define server logic required to draw a histogram
server <- function(input, output) {

  trans_filename <- reactive({
    path_file(input$trans_file$name) |>
      path_ext_remove() |>
      as.character()
  })

  output$names <- renderText({input$trans_file$name})

  ## Reactive transcripts object -----------------------------------------------
  trans <- reactive({

    out <- readLines(input$trans_file$datapath)

    ## Drop header -------------------------
    out <- out[3:length(out)]

    ## Split lines into types -------------

    type <- rep_len(1:4, length(out))

    out <- tibble(
      line_no   = out[type == 1],
      timestamp = out[type == 2],
      line_text = out[type == 3]
      )

    ## Write initials -------------------------------
    out <-
      mutate(out,
                  name  = str_extract(line_text, ".+(?=\\:)"),
                  line_text = str_remove(line_text, ".+\\:")
                  ) |>

      fill(name, .direction = "downup") |>

      rowwise() |>

      mutate(
        initials = str_extract_all(name, "[:upper:]", simplify = TRUE) |>
        as.vector() |>
        paste(collapse = "")
        ) |>

      ungroup()


    ## Group by continuous utterance ----------------
    utterances <-
      mutate(out,
             utterance = cumsum(name != lag(name, default = "-----"))
             ) |>
      group_by(utterance, name, initials) |>

      summarise(line_text = str_c(line_text, collapse = ""))

    ## Make names clever (i.e full name w/ initials, then just initials) --

    make_clever_names <- function(names, initials){
      out <- str_c(initials, ":")
      out[1] <- paste0(names, " (", initials, "):")
      out
    }

    utterances <-
      group_by(utterances, initials) |>

      mutate(
        clever_names = make_clever_names(name, initials),
        line_text = paste(clever_names, line_text)
        ) |>

      ungroup()

    ## Write text ---------------------------
    trans_text <-
      pull(utterances, line_text) |>
      str_c("\n") ## add linebreaks

    trans_text

  }) # Reactive object 'trans' ----

## Download handler -----------------------------------------------------------

  output$download_trans <- downloadHandler(
    filename = str_c(isolate(trans_filename()), "_cleaned.txt"),
    content = function(file) writeLines(trans(), file)
    )
}

# Run the application
shinyApp(ui = ui, server = server)
