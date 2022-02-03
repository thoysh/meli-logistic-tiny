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
  RV$upd_route <- 0
  RV$upd_event <- 0
  RV$upd_driver <- 0

  rac_routeDB <- reactive({
    RV$upd_route
    get_route()
  })
  
  rac_eventDB <- reactive({
    RV$upd_event
    get_event()
  })
  
  rac_driverDB <- reactive({
    RV$upd_driver
    get_driver()
  })
  
  callModule(module = routeServer,  "route", RV, rac_routeDB)
  callModule(module = eventServer,  "event", RV, rac_routeDB, rac_eventDB)
  callModule(module = driverServer, "driver", RV, rac_driverDB)
  callModule(module = reportServer, "report", RV)

}
