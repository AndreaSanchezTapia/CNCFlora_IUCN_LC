library(readxl)
library(textclean)
library(dplyr)
library("stringr")
library(flora)

treespp <- read_excel("./data/LeastConcern_BrazilEndemics_original.xlsx", sheet = 1)
names(treespp)
library(rgdal)
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

familias <- treespp$Family
especies <- treespp$ScientificName
library(flora)
especies <- purrr::map(especies, ~remove.authors(.)) %>% simplify2array()
treespp2 <- treespp %>% mutate(sp = purrr::map(ScientificName, ~remove.authors(.)) %>% simplify2array())
library(redlistr)
i <- 1
for (i in seq_along(especies)) {
nome_spfilt <- paste0("./output_centroides/",familias[i],"/",familias[i], "_",
                      especies[i],"_", "sp_filt.csv")
data_csv1 <- paste0("./output_centroides/",familias[i],"/",familias[i], "_",
                      especies[i],"_", "aoo_eoo.csv")
data_csv2 <- paste0("./output_centroides/aoo/",
                      especies[i],"_", "aoo_eoo.csv")
tabela.spfilt <- read.csv(nome_spfilt)
tabela <- tabela.spfilt[
    complete.cases(tabela.spfilt[,c("decimalLongitude", "decimalLatitude")]),]
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
# plot(eoo, add = F, border = "grey", lwd = 2)
# mapproj::map.grid(pts.utm)
# #mapproj::map.grid(alt.10km, nx = 5, ny = 5, col = "grey50", font = 1, cex = 0.7 , pretty = T)
# maps::map(,"Brazil",proj = 'mercator', add =F)

data <- data.frame(sp = especies[i],
                   familia = familias[i],
                   nusado = nrow(tabela),
                   eoo = eoo.area,
                   aoo2 = aoo2*4,
                   aoo10 = aoo10*100)
write.csv(data, file = data_csv1)
write.csv(data, file = data_csv2)
}
library(purrr)
tabla_aoo_eoo <- list.files("./output_centroides/aoo/", full.names = T) %>%
    purrr::map(.f = read.csv) %>%
    bind_rows() %>% dplyr::select(-1)
names(tabla_aoo_eoo)
tabla_final <- left_join(treespp2, tabla_aoo_eoo)
write.csv(tabla_final, file = "./output_centroides/aoo/aoo.csv")

plot(tabla_final$AOO_2x2km, tabla_final$aoo2)
plot(tabla_final$RecordsUsed, tabla_final$nusado)
plot(tabla_final$Area_10x10km, tabla_final$aoo10)
plot(tabla_final$EOO, tabla_final$eoo)
