library(readxl)
library(textclean)
library(dplyr)
library("stringr")
library(flora)
dir.create("aoo")
library(rgdal)
treespp <- read.csv("./results/names_flora.csv", row.names = 1)
names(treespp)

# alt <- raster("./data/raster/Raster_Brasil_altitude/Raster_Brasil_altitude.tif")
# alt <- raster(alt)
# alt.10km <- aggregate(x = alt, fact = 100)
# alt.10km <- setValues(alt.10km, 1)
# plot(alt.10km)
# alt.02km <- aggregate(x = alt, fact = 20)
# alt.02km <- setValues(alt.02km, 1)
# plot(alt.02km)

# res(alt)
# res(alt.02km)
# res(alt.10km)
# proj4string(alt.10km)
# alt.10km.utm <- projectRaster(alt.10km, res = 1000, crs = CRS("+init=epsg:29193"))
# plot(alt.10km.utm)
# res(alt.10km.utm)
any(is.na(treespp$nombre))
especies <- treespp$nombre
familias <- treespp$final_family
i <- 1
library(redlistr)

for (i in seq_along(especies)) {
    print(paste(especies[i], familias[i], "- calculating AOO and EOO", i, "of 673"))
    nome_spfilt <- paste0("./output_final/",familias[i],"/",familias[i], "_",
                      especies[i],"_", "sp_filt.csv")
    data_csv1 <-
        paste0("./output_final/", familias[i], "/", familias[i], "_", especies[i],"_", "aoo_eoo.csv")
    data_csv2 <- paste0("./aoo/", especies[i],"_", "aoo_eoo.csv")
    tabela.spfilt <- read.csv(nome_spfilt)
    tabela <- tabela.spfilt[complete.cases(tabela.spfilt[,c("decimalLongitude", "decimalLatitude")]),]
    pts <- SpatialPoints(tabela[,c("decimalLongitude", "decimalLatitude")])
    coordinates(tabela) <- ~ decimalLongitude + decimalLatitude
    proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")
    pts.utm <- spTransform(pts, CRS("+init=epsg:29193"))
    eoo <- makeEOO(pts.utm)
    eoo.area <- getAreaEOO(eoo)
    aoo2 <- getAOO(pts.utm, 2000)
    aoo10 <- getAOO(pts.utm, 10000)

# eoo.wgs <- spTransform(eoo, CRS("+proj=longlat +datum=WGS84"))
# plot(pts.utm, main = especies[i], add = F)
# plot(eoo, add = T, border = "grey", lwd = 2)
# mapproj::map.grid(pts.utm)
# #mapproj::map.grid(alt.10km, nx = 5, ny = 5, col = "grey50", font = 1, cex = 0.7 , pretty = T)
# maps::map(,"Brazil",proj = 'mercator', add =F)

data <- data.frame(sp = especies[i],
                   familia = familias[i],
                   nusado = nrow(tabela),
                   eoo = eoo.area,
                   aoo2 = aoo2,
                   aoo10 = aoo10)###não e necesario multiplicar
write.csv(data, file = data_csv1)
write.csv(data, file = data_csv2)
}

library(purrr)
tabla_aoo_eoo <- list.files("./aoo/", full.names = T) %>%
    purrr::map(.f = read.csv) %>%
    bind_rows() %>% dplyr::select(-1)
tabla_aoo_eoo <- tabla_aoo_eoo %>% rename(nombre = sp, family = familia)
dim(tabla_aoo_eoo)
names(tabla_aoo_eoo)
names(treespp)
tabla_final <- left_join(treespp, tabla_aoo_eoo)
head(tabla_final)
write.csv(tabla_final, file = "./results/final_with_aooeoo.csv")
tabla_final <- read.csv("./results/final_with_aooeoo.csv", row.names = 1)
names(tabla_final)
plot(tabla_final$aoo2, tabla_final$aoo10)
abline(b=1, a=0, col = "red")
plot(tabla_final$RecordsUsed, tabla_final$nusado)
plot(tabla_final$Area_10x10km, tabla_final$aoo10)
plot(treespp$EOO, tabla_final$eoo)


##
treespp <- readxl::read_excel("./data/LeastConcern_BrazilEndemics_original.xlsx", sheet = 1) %>%
    rename(scientificName = ScientificName) %>%
    mutate(nombre = purrr::map(scientificName, ~remove.authors(.)) %>%
               simplify2array())
