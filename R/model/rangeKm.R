
df_rangeKm <- data.frame(
  Intervalo = c("1/100", "101/150", "151/200", "201/250", "251/300", "301/350", "351/400", "401/450", "451/500"),
  Valor = c(1, 101, 151, 201, 251, 301, 351, 401, 451)
)

vec_rangeKm <- df_rangeKm %>% 
  arrange(Valor) %>% 
  add_row(Intervalo = .[1, 1], Valor = -Inf) %>% 
  add_row(Intervalo = .[1, 1], Valor = Inf) %>% 
  arrange(Valor) %>% 
  deframe()
