#filtrar o arquivo do inpa
library(readxl)
tabela_inpa_splink <- read_excel(path = "./data/planilha_geral_splink_inpa.xlsx", sheet = 1)

library(dplyr)
head(unique(tabela_inpa_splink$scientificname))
inpa_filtered <- tabela_inpa_splink %>% filter(tabela_inpa_splink$scientificname %in% especies)
write.csv(inpa_filtered, "./output/inpa_filtrado.csv")
