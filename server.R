## server.R ##
# Define a lógica do servidor do Shiny web app

# Define a lógica da sessão ----

# Define a lógica do servidor ----
server <- function(input, output, session) {
  setupFutilelogger(TRACE) # Setup futilelogger
  auth <- secure_server(check_credentials = check_credentials(credentials),
                        keep_token = T, timeout = 60) # Setup authentication
  RV <- reactiveValues()

# Tab Rotas ---------------------------------------------------------------

  # Preenche a tabela Histórico de Rotas
  output$tbl_routeHistoric <- DT::renderDT({
    iris2 <- iris
    colnames(iris2) <- paste0(colnames(iris2), "2")
    df <- cbind(iris, iris2) %>%
      fdtSetDf4js()

    df <- df %>%
      mutate(Evento = fshinySetInput(actionButton, df$Species, "btnDo",
                                     label = icon("search"),
                                     onclick = 'Shiny.setInputValue("sel_btnDo", this.id, {priority: "event"})'))
    
    datatableTy(df, nome = "Rotas",
                class = "nowrap",
                extensions = "FixedColumns",
                escape = 0,
                options = list(scrollX = T,
                               fixedColumns = list(leftColumns = 2)),
                callback = JS("table.order([5, 'desc']).draw();"))
  })
}
