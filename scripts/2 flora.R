library(readr)
library(readxl)
library(dplyr)
library(flora)
#read flora data
flora <- read_csv("./ipt/all_flora.csv") %>% select(-1)

# read original data----
treespp <- read_excel("./data/LeastConcern_BrazilEndemics_original.xlsx", sheet = 1) %>%
    rename(scientificName = ScientificName) %>%
    mutate(nombre = purrr::map(scientificName, ~remove.authors(.)) %>%
               simplify2array())



#los nombres están aceitos en la flora?
taxon.filt <- filter(flora, nombre %in% treespp$nombre,
                     taxonRank == "ESPECIE")
setdiff(taxon.filt$nombre, treesspp2$final_name)
count(taxon.filt, taxonomicStatus, nomenclaturalStatus)
aceitos <- taxon.filt %>% filter(taxonomicStatus == "NOME_ACEITO")
#reads relationship
relationship <- read_delim("./ipt/resourcerelationship.txt",
                           delim = "\t", quote = "") %>%
    distinct()

sinonimos <- taxon.filt %>%
    filter(taxonomicStatus != "NOME_ACEITO") %>%
    left_join(relationship) %>% group_by(id) %>%

    mutate(sinonimo_de = relatedResourceID)
sin.filt <- filter(flora, id %in% sinonimos$sinonimo_de) %>%
    mutate(id_correct = id)
#toca hacer un loop de sinonimos
#vector de nombres
#quien es ok, ok,
#quien es sinonimo, sinonimo,
#hasta que sean todos ok.
taxon.filt <- filter(flora, nombre %in% treespp$nombre,
                     taxonRank == "ESPECIE") %>%
    #select(id, acceptedNameUsage, scientificName, nomenclaturalStatus) %>%
    #filter(!is.na(acceptedNameUsage))
#taxon.filt %>%
    #select(id, nombre, taxonomicStatus, nomenclaturalStatus) %>%
    mutate(final_correct_name =
               ifelse(!is.na(acceptedNameUsage), acceptedNameUsage, scientificName)) %>% select(2, 6, 7,8,family)

taxon.filt %>% View()#
#una tabla para hacer el loop por ese estatus

relaciones <- flora %>%
    select(taxonID, scientificName, nomenclaturalStatus, taxonomicStatus, acceptedNameUsageID, acceptedNameUsage, parentNameUsage, parentNameUsageID)



treesspp2 <- treespp %>%
    select(scientificName, nombre) %>%
    left_join(flora) %>%
    select(scientificName,
           taxonID,
           nombre,
           acceptedNameUsageID,
           acceptedNameUsage,
           parentNameUsage,
           family,
           taxonomicStatus,
           nomenclaturalStatus,
           occurrenceRemarks,
           location,
           lifeForm,
           habitat,
           locality) %>%
    mutate(final_name = ifelse(taxonomicStatus == "NOME_ACEITO", scientificName, acceptedNameUsage)) %>%
    mutate(final_name = ifelse(is.na(taxonID) & is.na(acceptedNameUsageID), "not found in Flora do Brasil",final_name)) %>%
    select(scientificName, final_name, family, occurrenceRemarks, location, lifeForm, habitat, locality)

write.csv(treesspp2, "./results/names_flora.csv")
View(treesspp2)
