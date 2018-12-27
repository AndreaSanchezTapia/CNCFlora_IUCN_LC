# 9.areas protegidas
library(rgdal)
library(dplyr)
uc <- readOGR(dsn = "./data/shape/UC/", layer = "ucstodas")
proj4string(uc) <-  CRS("+init=epsg:4618")
spp <- read.csv("./results/aoo_veg_cites.csv", row.names = 1)
spp$nombre
especies <- spp$nombre
familias <- spp$final_family

for (i in seq_along(especies)) {
    print(paste(especies[i], familias[i], "- analyzing presence in UCs", i, "of 673"))
    nome_spfilt <- paste0("./output_final/",familias[i],"/",familias[i], "_",
                          especies[i],"_", "sp_filt.csv")
    uc_output <-
        paste0("./output_final/", familias[i], "/", familias[i], "_", especies[i],"_", "UC.csv")

    tabela.spfilt <- read.csv(nome_spfilt, row.names = 1)
    tabela <- tabela.spfilt[complete.cases(tabela.spfilt[,c("decimalLongitude", "decimalLatitude")]),]
    pts <- SpatialPoints(tabela[,c("decimalLongitude", "decimalLatitude")])
    coordinates(tabela) <- ~ decimalLongitude + decimalLatitude
    proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")
    pts.sad69 <- spTransform(pts, CRS("+init=epsg:4618"))
    over_results <- sp::over(pts.sad69, uc)
    n_in_uc <- over_results %>% filter(complete.cases(.)) %>% count() %>% pull
    which_uc <- over_results %>%
        filter(complete.cases(.)) %>%
        dplyr::select(NOME_UC1) %>%
        distinct() %>%
        summarise(uc = unique(paste(NOME_UC1, collapse = ",")))
    final_res <- data.frame(nombre = especies[i],
                            final_family = familias[i],
                            nusado = nrow(tabela),
                            n_in_uc = n_in_uc,
                            uc = which_uc)
    write.csv(final_res, file = uc_output)
}


library(purrr)
tabla_UCs <- list.files("./output_final", full.names = T, pattern = "UC.csv$", recursive = T) %>%
    purrr::map(.f = readr::read_csv) %>%
    bind_rows() %>%
    dplyr::select(-1)
tabla_UCs

write.csv(tabla_UCs, "./results/tabla_UCs.csv")
