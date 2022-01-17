
routeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tabBox(title = "Rotas", width = 12, id = "tabRotas", 
        tabPanel("Histórico de Rotas", 
                 withSpinnerTy(DT::DTOutput(ns("tbl_routeHistoric")))
                 ),
        
        tabPanel("Importar Rotas", 
                 p("Para importar as rotas, selecione o Service Center e os arquivos .json, em seguida, clique em 'Importar rotas'."),
                 selectInput(ns("inSel_serviceCenter"), "Service Center *", choices = vec_serviceCenter),
                 fileInput(ns("inFile_routes"), "Arquivo .json das rotas",
                           multiple = T,
                           accept = c("text/plain", ".json"),
                           buttonLabel = "Buscar...",
                           placeholder = "Nenhum arquivo selecionado"),
                 withSpinnerTy(DT::DTOutput(ns("tbl_routeUpload"))), br(),
                 actionButton(ns("btn_importRoutes"), "Importar rotas")
                 ),
        
        tabPanel("Histórico de Importação", 
                 withSpinnerTy(DT::DTOutput(ns("tbl_routeUploaded")))
                 )
      )
    )
  )
}

routeServer <- function(id, RV, rac_routeDB) {
  moduleServer(id, function(input, output, session) {
    
    ## OUTPUT Histórico de Rotas
    output$tbl_routeHistoric <- DT::renderDT({
      req(rac_routeDB())
      df <- rac_routeDB()
      
      datatableTy(df, nome = "Rotas",
                  class = "nowrap",
                  extensions = "FixedColumns",
                  escape = 0,
                  options = list(scrollX = T,
                                 fixedColumns = list(leftColumns = 2)),
                  callback = JS("table.order([5, 'asc']).draw();")) %>% 
        formatCurrency(names(df) %in% c("DistEstimada", "DistRealizada"), 
                       currency = "", mark = ".", dec.mark = ",", digits = 2) %>%
        formatCurrency(names(df) %in% c("Performance"), 
                       currency = "", mark = ".", dec.mark = ",", digits = 3) %>%
        formatDate(names(Filter(is.Date, df)), method = "toLocaleDateString", params = "pt-BR") %>% 
        formatDate(names(Filter(is.POSIXct, df)), method = "toLocaleString", params = "pt-BR")
    })
    
    ## OUTPUT Upload de Rotas (para verificar data antes de importar)
    output$tbl_routeUpload <- DT::renderDT({
      req(input$inFile_routes)
      req(input$inSel_serviceCenter)
      
      df <- input$inFile_routes %>%
        .$datapath %>%
        map_df(~fromJSON(., flatten = T)) %>%
        transmute(
          Data = (`initDate` /24/60/60) %>% as.Date(origin = "1970-01-01") %>% as.character()
        ) %>%
        count(Data) %>%
        transmute(`ServiceCenter` = input$inSel_serviceCenter,
                  Data = Data %>% as.Date(),
                  Rotas = n) %>%
        as.data.frame()
      
      datatableTy(df, nome = "Upload", dom = "rt",
                  class = "nowrap",
                  options = list(pageLength = nrow(df)),
                  callback = JS("table.order([1, 'desc']).draw();")) %>%
        formatDate(names(Filter(is.Date, df)), method = "toLocaleDateString", params = "pt-BR")
    })
    
    ## BOTAO salva uploads no BD
    observeEvent(input$btn_importRoutes, {
      files <- input$inFile_routes
      if (is.null(files)) {
        return()
      } else {
        nfiles = nrow(files)
        for (i in 1:nfiles) {
          tryCatch({
            df <- etl_routeJson2Df(fileJson = files$datapath[i],
                                    serviceCenter = input$inSel_serviceCenter)
            del_route(data = df$Data[1], SC = input$inSel_serviceCenter, user = RV$user)
            set_route(df, user = RV$user)
            shinyalertTy("Sucesso na importação", type = "success")
          }, error = function(e) {
            flog.error("btn_importRoutes (E) | %s", e$message)
            shinyalertTy("Erro de importação", paste0("Arquivo: ", files$name[i]), type = "error")
          })
        }
        RV$upd_route <- RV$upd_route + 1
      }
    })
    
    ## OUTPUT Histórico de Uploaded de Rotas (para verificar rotas importadas)
    output$tbl_routeUploaded <- DT::renderDT({
      req(rac_routeDB())
      
      df <- get_routeUploadHistory() %>% 
        arrange(desc(CriadoEm)) %>%
        as.data.frame()
      
      datatableTy(df, nome = "Importação de Rotas",
                  class = "nowrap",
                  escape = 0,
                  # options = list(pageLength = nrow(df)),
                  callback = JS("table.order([1, 'desc']).draw();")) %>%
        formatDate(names(Filter(is.POSIXct, df)), method = "toLocaleString", params = "pt-BR")
    })
    
  })
}
