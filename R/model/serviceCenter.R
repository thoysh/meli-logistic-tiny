
df_serviceCenter <- data.frame(
  Sigla = c("SMG1", "EMG9", "SMG8", "SMS1", "SDF1", "SMG2"),
  Cidade = c("CONTAGEM", "LAFAIETE", "VESPASIANO", "CAMPO GRANDE", "BRASILIA", "JUIZ DE FORA")
)

vec_serviceCenter <- df_serviceCenter %>%
  mutate(Opcao = paste(Sigla, "-", Cidade)) %>%
  arrange(Opcao) %>% 
  select(Opcao, Sigla) %>% 
  deframe()