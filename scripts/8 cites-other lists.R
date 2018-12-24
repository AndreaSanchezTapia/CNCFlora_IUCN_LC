#8cites y especies amenazadas
aoo <- read.csv("./results/aoo_veg.csv", row.names = 1)
#read data
#unzip("./data/ameacadas/fwdplanosdeaonacionaispublicadospelocncflora.zip", exdir = "./data/ameacadas/")

##########FORMAT

library(readxl)
library(flora)



espi <- read_excel("./data/ameacadas/Lista especies ameacadas pan-serraespinhacomeridional.xlsx") %>%
    set_names(c("Familia","Nome_cientifico", "Categoria")) %>%
    mutate(source = "espi") %>% filter(!is.na(Familia))
tail(espi)
espi$Nome_cientifico
fav <- read_excel("./data/ameacadas/Lista de especies ameacadas pan-faveiro-de-wilson.xlsx") %>%
    set_names(c("Familia","Nome_cientifico", "Categoria")) %>%
    mutate(Nome_cientifico = purrr::map(Nome_cientifico, ~remove.authors(.)) %>%
            simplify2array()) %>% mutate(source = "fav")
fav$Nome_cientifico[3] <- "Anemopaegma arvense"
fav$Nome_cientifico

grao <- read_excel("./data/ameacadas/Lista de especies ameacadas de extincao contempladas pelo pan grao mogol-francisco-sa.xlsx")  %>%
    set_names(c("Familia","Nome_cientifico", "Categoria")) %>%
    mutate(Familia = tolower(Familia)) %>%
    mutate(Familia = Hmisc::capitalize(Familia)) %>%
    mutate(source = "grao")
grao$Nome_cientifico

lagoas <- read_excel("./data/ameacadas/Lista de especies ameacadas Pan Lagoas do Sul.xlsx") %>%
    dplyr::select(-3) %>%
    set_names(c("Familia","Nome_cientifico", "Categoria")) %>%
    mutate(Familia = tolower(Familia)) %>%
    mutate(Familia = Hmisc::capitalize(Familia)) %>%
    mutate(source = "lagoas")

lagoas$Nome_cientifico

rj <- read_excel("./data/ameacadas/lista_ameacadas_pan_RJ.xlsx") %>%
    set_names(c("Familia","Nome_cientifico", "Categoria")) %>%
    mutate(Familia = tolower(Familia)) %>%
    mutate(Familia = Hmisc::capitalize(Familia)) %>%
    mutate(source = "rj")
rj$Nome_cientifico

all_CNCFlora <- full_join(espi, fav) %>% full_join(grao) %>% full_join(lagoas) %>% full_join(rj)
all_CNCFlora %>% arrange(Familia, Nome_cientifico) %>% View()
all_CNCFlora <- all_CNCFlora %>% group_by(Nome_cientifico, Categoria) %>%
    mutate_at(.funs = function(x) paste(x, collapse = "-"), 4) %>% distinct() %>% ungroup()
write.csv(all_CNCFlora, "./data/ameacadas/all_CNCFLora.csv")

#######################

CNC <- read.csv("./data/ameacadas/all_CNCFLora.csv", row.names = 1) %>%
    rename(nombre = Nome_cientifico, CNCFlora = Categoria) %>%
    dplyr::select(-1)

any(aoo$nombre %in% CNC$nombre)
#nothing in CNCFlora

#####CITES
cites <- read.csv("./data/CITES/Index_of_CITES_Species_2018-12-19 10_47.csv")
cites <- cites %>% filter(Kingdom == "Plantae") %>% filter(RankName %in% c("SPECIES", "SUBSPECIES", "VARIETY"))
which(aoo$nombre %in% cites$FullName)
aoo$nombre[195]
cites[cites$FullName == "Dalbergia brasiliensis",] %>% View()
unique(cites$RankName)
head(cites)

aoo <- aoo %>% mutate(CITES = ifelse(nombre %in% cites$FullName, "yes (Appendix II)", ""))
aoo[aoo$CITES == "yes (Appendix II)",]

write.csv(aoo, "./results/aoo_veg_cites.csv")
