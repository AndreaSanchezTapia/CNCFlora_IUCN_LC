# carrega pacotes
library(dplyr)
library("stringr")
library("rgbif")
library(readxl)
library(textclean)
library(flora)
library(lubridate)
#devtools::install_github("diogosbr/spfilt")
library(spfilt)
# le tudo
treespp <- read.csv("./results/names_flora.csv", row.names = 1)
familias <- treespp$final_family
especies <- treespp$nombre


#primeiro ler a planilha com a lista de coordenadas por centroide----
tabela_centroides <- read.delim(file = "./data/centroide_municipio.csv",
                                header = TRUE, sep = ";",
                                stringsAsFactors = FALSE,
                                fileEncoding = "ISO-8859-9")
# tira todos os caracteres e bota minuscula
tabela_centroides <- tabela_centroides %>%
    mutate(municipality = replace_non_ascii(tolower(NOME)),
           stateProvince = replace_non_ascii(tolower(NOMEUF)))
#centroides ucs----
tabela_centroides_ucs <- read.delim(file = "./data/centroide_uc.csv",
                                    header = TRUE, sep = ";",
                                    stringsAsFactors = FALSE,
                                    fileEncoding = "ISO-8859-9")
tabela_centroides_ucs <- tabela_centroides_ucs %>%
    mutate(uc = replace_non_ascii(tolower(NOME_UC1)))

#shape estados----
library(rgdal)
estados <- rgdal::readOGR(dsn = "./data/shape/Limites_v2017/", layer = "lim_unidade_federacao_a")
#tabela com a sigla pois uma limpeza é substituir a sigla ("rj") pelo nome completo
sigla_estados <- estados@data[,c("nome", "sigla")] %>% data.frame() %>%
    mutate(stateProvince = replace_non_ascii(tolower(nome))) %>%
    mutate(sigla = replace_non_ascii(tolower(sigla)))
#centroides estados----
#já deixa em minusculo sem acento tudo
centroides_estados <-
    rgeos::gCentroid(estados, byid = T, id = estados$nome) %>%
    data.frame %>%
    tibble::rownames_to_column(var = "estados_shp") %>%
    mutate(stateProvince = replace_non_ascii(tolower(estados_shp))) %>%
    rename(x_estado = x, y_estado = y) %>% left_join(sigla_estados)

centroides_estados

#compara o nome dos estados e dos municipios porque há municipios com o mesmo nome de alguns estados, importante para a limpeza----
setdiff(centroides_estados$stateProvince, tabela_centroides$municipality)
#mesmo nome estado e municipio
tabela_centroides[which(tabela_centroides$municipality %in%  centroides_estados$stateProvince),]
dupl_names_state_city <- tabela_centroides$municipality[which(tabela_centroides$municipality %in%  centroides_estados$stateProvince)]
#nomes de estado seguros
non_dupl_names <- setdiff(centroides_estados$stateProvince, dupl_names_state_city)

#nomes unicos de municipio
#hay 5570municipios, 282 son duplicados
unique_mpo <- tabela_centroides %>% distinct(municipality) %>% pull()
dupl_mpo <- tabela_centroides$municipality[duplicated(tabela_centroides$municipality)]
mpo_estado_unico <- setdiff(unique_mpo, dupl_mpo)

#assignação de centroides----
especies

#cria um vetor vazio para ficar de olho em algumas espécies que ainda tem NA nas notas.

for (i in 1:length(especies)) {
    print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))

    nome_clean <- paste0("./output_final/",familias[i],"/",familias[i], "_", especies[i],"_",
                         "clean.csv")
    nome_centroides <- paste0("./output_final/",familias[i],"/",familias[i], "_", especies[i],"_",
                         "centroides.csv")
    tabela_especie <- read.csv(nome_clean, row.names = 1, stringsAsFactors = F) %>%
        mutate(catalogNumber = factor(catalogNumber))

    #corrige nombres de estados
    #corrige siglas
    substituir_siglas <- function(x) {
        x1 <- textclean::replace_non_ascii(tolower(x))
        if (any(sigla_estados$sigla %in% x1)) {
            return(as.character(sigla_estados$nome[which(sigla_estados$sigla == x1)]))
        } else {
            as.character(x)
        }
    }

    tabela_especie$stateProvince <-
        purrr::map(tabela_especie$stateProvince, .f = substituir_siglas) %>%
        simplify2array()
    #igual com o municipio
    tabela_especie_edit <- tabela_especie %>%
        rename(municipality.original = municipality) %>%
        rename(stateProvince.original = stateProvince) %>%
        rename(country.original = country) %>%

    mutate(municipality = replace_non_ascii(tolower(municipality.original)),
           stateProvince = replace_non_ascii(tolower(stateProvince.original)),
           country = replace_non_ascii(tolower(country.original))) %>%
#corrige espirito santo
                mutate(stateProvince = ifelse(stateProvince == "espa-rito santo",
                                      "espirito santo",
                                      stateProvince)) %>%
        #corrige estados na casa de municipios
        mutate(municipality = ifelse(municipality %in% c("brasil", "brazil", non_dupl_names), NA, municipality))


#junta con centroides
    tabela_especie_edit <- tabela_especie_edit %>%
        left_join(tabela_centroides) %>%
        left_join(centroides_estados)
#assignar centroides
        tabela_corrigida <- tabela_especie_edit %>%
            # cria as colunas
mutate(new_Lat = NA, new_Lon = NA, notes = NA) %>%
            # quando é numérico e não é zero
            mutate(new_Lat = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                                     decimalLatitude, new_Lat),
                   new_Lon = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                                     decimalLongitude, new_Lon),
                   notes = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                                  "original coordinates", notes)) %>%
            #cuando existen y valen cero
            mutate(new_Lat = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 & !is.na(municipality) &
                    !is.na(stateProvince),
                POINT_Y,
                new_Lat),
                   new_Lon = ifelse(
                decimalLatitude == 0 &
                    decimalLongitude == 0 &
                    !is.na(municipality) &
                   !is.na(stateProvince),
                POINT_X,
                new_Lon),
                notes = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 &  !is.na(municipality) &
                    !is.na(stateProvince),
                "centroide mpo (0)",
                notes)) %>%
            #cuando existen y valen cero
            # mutate(new_Lat = ifelse(
            #     decimalLatitude == 0 & decimalLongitude == 0 & is.na(municipality) &
            #         !is.na(stateProvince),
            #     y_estado,
            #     new_Lat),
            #        new_Lon = ifelse(
            #     decimalLatitude == 0 & decimalLongitude == 0 &  is.na(municipality) &
            #        !is.na(stateProvince),
            #     x_estado,
            #     new_Lon),
            #     notes = ifelse(
            #     decimalLatitude == 0 & decimalLongitude == 0 &  is.na(municipality) &
            #         !is.na(stateProvince),
            #     "centroide estado (0)",
            #     notes)) %>%
        #quando não existe , bota o estado
            #mutate(new_Lat = ifelse(
            #    is.na(decimalLatitude) & is.na(decimalLongitude) &
            #is.na(municipality) &
             #       !is.na(stateProvince), y_estado, new_Lat),
              #  new_Lon = ifelse(
               #     is.na(decimalLatitude) & is.na(decimalLongitude) &  is.na(municipality) &
                #        !is.na(stateProvince), y_estado, new_Lon),
                #notes = ifelse(
                 #   is.na(decimalLatitude) & is.na(decimalLongitude) &  is.na(municipality) &
                  #      !is.na(stateProvince), "centroide estado", notes)) %>%
            #quando não existe mas tem municipio , bota o municipio
            mutate(new_Lat = ifelse(
                is.na(decimalLatitude) & is.na(decimalLongitude) &  !is.na(municipality) &
                    !is.na(stateProvince), POINT_Y, new_Lat),
                new_Lon = ifelse(
                    is.na(decimalLatitude) & is.na(decimalLongitude) &  !is.na(municipality) &
                        !is.na(stateProvince), POINT_X, new_Lon),
                notes = ifelse(
                    is.na(decimalLatitude) & is.na(decimalLongitude) &  !is.na(municipality) &
                        !is.na(stateProvince), "centroide mpo", notes)) %>%
            mutate(notes = ifelse(
                decimalLatitude == 0 & decimalLongitude == 0 & is.na(municipality) &
                    is.na(stateProvince),
                "no coordinates, municipality or state provided (0)", notes)) %>%
            mutate(notes = ifelse(
                    is.na(decimalLatitude) & is.na(decimalLongitude) &  is.na(municipality) &
                        is.na(stateProvince),
                    "no coordinates, municipality or state provided", notes)) %>%
    mutate(new_Lat = ifelse(is.na(notes) & municipality %in% unique_mpo, POINT_Y, new_Lat),
           new_Lon = ifelse(is.na(notes) & municipality %in% unique_mpo, POINT_X, new_Lon),
           notes = ifelse(is.na(notes) & municipality %in% unique_mpo, "centroide mpo (no state)", notes)) %>%
            mutate(new_Lat = ifelse(is.na(notes) & municipality %in% dupl_mpo, POINT_Y, new_Lat),
                   new_Lon = ifelse(is.na(notes) & municipality %in% dupl_mpo, POINT_X, new_Lon),
                   notes = ifelse(is.na(notes) & municipality %in% dupl_mpo, "check coordinate: municipality name is duplicated", notes))


print(count(tabela_corrigida, notes))

#los cambios que pidió mary
tabela_corrigida2 <- tabela_corrigida %>%
    mutate(
        municipality = municipality.original,
        stateProvince = stateProvince.original,
        decimalLongitude = new_Lon,
        decimalLatitude = new_Lat,
        comments = notes
    )

tabela_corrigida2[is.na(tabela_corrigida2)] <- ""

    #para checar o resultado
    write.csv(tabela_corrigida2, file = nome_centroides)
}



####sp_filt de Diogo
mpos <- rgdal::readOGR(dsn = "./data/shape/Limites_v2017/", layer = "lim_municipio_a")
tabela_centroides_2 <- tabela_centroides %>%
    rename(geocodigo = GEOCODIGO, nome = NOME) %>%
    mutate(geocodigo = as.factor(geocodigo))
mpos@data <- left_join(mpos@data, tabela_centroides_2)
proj4string(mpos)
mpos2 <- sp::spTransform(mpos, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# names(mpos2)
# dim(mpos@data)

shape.municipios <- mpos2

source('~/Documents/2 Coleguinhas/Diogo/sp_filt/R/filt_andrea.R')
#72
#158
#179
#226
#279
#373
for (i in 1:length(especies)) {
#for (i in c(72,158,179,226,279,373)) {
    print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
    nome_centroides <- paste0("./output_final/",familias[i],"/",familias[i], "_", especies[i],"_",
                              "centroides.csv")
    nome_spfilt <- paste0("./output_final/",familias[i],"/",familias[i], "_", especies[i],"_",
                              "sp_filt.csv")
    tabela_especie <- read.csv(nome_centroides, row.names = 1, stringsAsFactors = F)

    tabela_especie2 <- tabela_especie %>% dplyr::mutate(ID = row_number())

    tabela_sppfilt <- tabela_especie2 %>%
        dplyr::select("ID", "scientificName", 'decimalLongitude', 'decimalLatitude', 'municipality', "stateProvince") %>%
        rename(species = scientificName,
               lon = decimalLongitude,
               lat = decimalLatitude,
               adm1 = stateProvince)

        tabela_sppfilt <- tabela_sppfilt[complete.cases(cbind(tabela_sppfilt$lon, tabela_sppfilt$lat)),]


    sp_filt_res <- filt_andrea(pts = tabela_sppfilt,
                           inverted = T,
                        shape.municipios = mpos2)
print(nrow(tabela_especie))
print(nrow(tabela_especie2))
print(nrow(tabela_sppfilt))
print(nrow(sp_filt_res))
sp_filt_res <- sp_filt_res %>% rename(scientificName = species,
                                      decimalLongitude = lon,
                                      decimalLatitude = lat,
                                      lowercase_municipality = county.original)
tabela_especie2 <- tabela_especie2 %>%
    mutate(lowercase_municipality = textclean::replace_non_ascii(tolower(municipality)))

resultado_final <- left_join(tabela_especie2, sp_filt_res) %>%
    mutate(comments = paste(comments, filt)) %>%
    mutate(comments = ifelse(filt == "outside municipality",
                             paste(comments, "original in", lowercase_municipality, "falls in", county.shape),
                             comments)) %>%
    dplyr::select(one_of(names(tabela_especie)))

if (nrow(resultado_final) != nrow(tabela_especie)) stop()
        write.csv(resultado_final, nome_spfilt)

        }
#72
#158
#179
#226
#279
#373
#asigné cero para que siguiera la lipieza
