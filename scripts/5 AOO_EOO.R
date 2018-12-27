library(readxl)
library(textclean)
library(dplyr)
library("stringr")
library(flora)
library(red)
library(ConR)
dir.create("aoo")
library(rgdal)
treespp <- read.csv("./results/names_flora.csv", row.names = 1)
names(treespp)
a <- maps::map(,c("Brazil","Mexico"))
a$range
b <- maps::map(add = T)
b$range

pais <- readOGR("./data/shape/Limites_v2017/", "lim_pais_a")
rgdal::dissolve(dissolve)
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
    coord <- tabela[,c("decimalLongitude", "decimalLatitude")]
    pts <- SpatialPoints(coord)
    #coordinates(tabela) <- ~ decimalLongitude + decimalLatitude
    proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")
    pts.utm <- spTransform(pts, CRS("+init=epsg:29193"))
    eoo <- makeEOO(pts.utm)
    eoo_area <- getAreaEOO(eoo)
    eoo_conr <- ConR::EOO.computing(coord, Name_Sp = especies[i], write_shp = T)
    points(coord)
    aoo2 <- getAOO(pts.utm, 2000)
    aoo10 <- getAOO(pts.utm, 10000)
    red_aoo2 <- red::aoo(pts.utm@coords)

# eoo.wgs <- spTransform(eoo, CRS("+proj=longlat +datum=WGS84"))
# plot(pts.utm, main = especies[i], add = F)
# plot(eoo, add = T, border = "grey", lwd = 2)
# mapproj::map.grid(pts.utm)
# #mapproj::map.grid(alt.10km, nx = 5, ny = 5, col = "grey50", font = 1, cex = 0.7 , pretty = T)
# maps::map(,"Brazil",proj = 'mercator', add =F)

data <- data.frame(nombre = especies[i],
                   final_family = familias[i],
                   nusado = nrow(tabela),
                   eoo = eoo_area,
                   eoo_conr = eoo_conr$EOO,
                   aoo2 = aoo2,
                   red_aoo2 = red_aoo2,
                   aoo10 = aoo10)###não e necesario multiplicar
write.csv(data, file = data_csv1)
write.csv(data, file = data_csv2)
}

library(purrr)
tabla_aoo_eoo <- list.files("./aoo", full.names = T) %>%
    purrr::map(.f = read.csv) %>%
    bind_rows() %>% dplyr::select(-1)
names(tabla_aoo_eoo)
dim(tabla_aoo_eoo)
names(treespp)
tabla_final <- left_join(treespp, tabla_aoo_eoo)
tabla_final$eoo
head(tabla_final)
write.csv(tabla_final, file = "./results/final_with_aooeoo.csv")


### Comparando----
tabla_final <- read.csv("./results/final_results.csv", row.names = 1)
original_table <- readxl::read_excel("./data/LeastConcern_BrazilEndemics_original.xlsx", sheet = 1) %>%

    rename(scientificName = ScientificName) %>%
    mutate(nombre = purrr::map(scientificName, ~remove.authors(.)) %>%
               simplify2array())
data <- left_join(tabla_final, original_table, by = c("nombre"))
dim(data)
library(dplyr)
library(magrittr)
data %<>% mutate(aoo2 = 4*aoo2, aoo10 = 100*aoo10)
write.csv(data, file = "./results/final_results_correctedAOO.csv")


any(is.na(data$aoo2))
any(is.na(data$aoo10))
data$AOO_2x2km
library(ggplot2)

data %>% ggplot2::ggplot(aes(x = AOO_2x2km, y = aoo2)) +
    geom_abline(slope = 1, intercept = 0, col = "red") +
    geom_point() +
    ggtitle("AOO2 us+them")+
    coord_equal()+
    theme_classic()

data %>% ggplot2::ggplot(aes(x = Area_10x10km, y = aoo10)) +
    geom_abline(slope = 1, intercept = 0, col = "red") +
    geom_point() +
    ggtitle("AOO10 us+them")+
    coord_equal()+
    theme_classic()

data %>% ggplot2::ggplot(aes(x = EOO, y = eoo)) +
    geom_abline(slope = 1, intercept = 0, col = "red") +
    geom_point() +
    ggtitle("EOO us+them")+
    coord_equal()+
    theme_classic()

data %>% ggplot2::ggplot(aes(x = aoo2, y = aoo10)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    geom_abline(slope = 1, intercept = 0, col = "red") +
    ggtitle("AOO2 vs AOO 10 US")+
#    coord_equal()+
    theme_classic()

data %>% ggplot2::ggplot(aes(x = AOO_2x2km, y = Area_10x10km)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    geom_abline(slope = 1, intercept = 0, col = "red") +
    ggtitle("AOO2 vs AOO 10 THEM")+
    theme_classic()


data %>% ggplot2::ggplot(aes(x = RecordsUsed, y= nusado)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    geom_abline(slope = 1, intercept = 0, col = "red") +
    ggtitle("Records used") +
    coord_equal()

data %>% ggplot2::ggplot(aes(x = RecordsUsed, y= Area_10x10km)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    ggtitle("Records used")

data %>% ggplot2::ggplot(aes(x = nusado, y= aoo10)) +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    ggtitle("Records used") +
    theme_classic()


##

#install.packages("red")
#install.packages("ConR")
library(red)
??red
red_aoo <- red::aoo(tabela[,c("decimalLongitude", "decimalLatitude")])
ConR
