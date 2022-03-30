#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>%
#' @noRd
app_server <- function(input, output, session) {
  mod_aa_plot_server("aa_plot_1")
  mod_dna_peptide_server("dna_peptide_1")
  # Your application server logic
  output$DNA <- renderUI({
    textAreaInput(
      inputId = ns("DNA"),
      label = "DNA sequence",
      placeholder = "Insert DNA sequence",
      value = dna(),
      height = 100,
      width = 600
    )
  })
observeEvent(input$generate_dna, {
    dna(
      Group2Package::sample_with_replacement(input$nucleotide_size)
    )
  })
output$peptide <- renderText({
  # Ensure input is not NULL and is longer than 2 characters
  if(is.null(input$DNA)){
    NULL
  } else if(nchar(input$DNA) < 3){
    NULL
  } else{
    input$DNA %>%
      toupper() %>%
      Group2Package::codon_start() %>%
      Group2Package::TU_sub() %>%
      Group2Package::translation()
  }
})
}


