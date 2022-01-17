## server.R ##
# Define a lógica do servidor do Shiny web app

# Define a lógica da sessão ----

# Define a lógica do servidor ----
server <- function(input, output, session) {
  setupFutilelogger(TRACE) # Setup futilelogger
  auth <- secure_server(check_credentials = check_credentials(credentials),
                        keep_token = T, timeout = 60) # Setup authentication
  
  RV <- reactiveValues()
  RV$user <- "teste" #auth$user

  rac_routeDB <- reactive({
    RV$upd_route
    df <- get_route()
    if ("Data" %in% names(df))
      df$Data <- df$Data %>% as.Date()
    df
  })
  
# Tab Rotas ---------------------------------------------------------------
  routeServer("route", RV, rac_routeDB)
  
}
