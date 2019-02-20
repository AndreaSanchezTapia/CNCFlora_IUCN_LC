#### Script BGCI unificado
#le tudo ----
library(dplyr)
library("stringr")
library("rgbif")
library(readxl)
source("scripts/new_duplicated.R")
treespp <- read.csv("./results/names_flora.csv", row.names = 1)
treespp$nombre
familias <- treespp$final_family
especies <- treespp$nombre

#extração de registros por espécie do gbif
dir.create("output_final")

#buscando registros de gbif----
#i <- 371
#por algun motivo no sirve el 371 sin nombtre

for (i in 1:length(especies)) {
    dir.create(paste0("./output_final/",familias[i]), showWarnings = F)
    nome_arquivo <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "raw.csv")
    print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))

    if (!file.exists(nome_arquivo)) {
    key <- name_backbone(name = treespp$scientificName[i])$speciesKey
    if (!is.null(key)) {
        occs <- list()
        for (k in 1:length(key)) {
            occs[[k]] <- occ_search(
                taxonKey = key[k],
                limit = 100000,
                hasCoordinate = TRUE,
                basisOfRecord = "PRESERVED_SPECIMEN",
                hasGeospatialIssue = F,
                return = 'data'
                #fields = "minimal"
            )
        }
        print(lapply(occs,dim))
        if (any(!is.null(lapply(occs,dim)))) {
            dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
            occs.f <- subset(occs, dim.null == T)
            occs.f <- dplyr::bind_rows(occs.f)
            print(dim(occs.f))
            write.csv(occs.f, nome_arquivo)
        }
    } else {
        warning(paste("No key found for", especies[i], "\n"))
    }
    }
    #query.i <- occ_search(scientificName = especies[i])

        #as colunas que criam os comentários nem sempre existem:

}


#limpar a coluna de coletor do inpa----
#le inpa----
tabela_inpa_splink <- read.csv(file = "./results/output_centroides/inpa_filtrado.csv", header = TRUE)
# a coluna acceptedNameUsage não existe e a coluna scientifiName nãõ tem autor
unique(tabela_inpa_splink$institutioncode)

tabela_inpa_splink <- tabela_inpa_splink %>%
    mutate(institutioncode = "Instituto Nacional de Pesquisas da Amazônia (INPA)")
#todos os coletores do inpa
colectores_inpa <- tabela_inpa_splink %>% dplyr::select(institutioncode, catalognumber, collector) %>% mutate(collector = as.character(collector)) %>% rename(catalogNumber = catalognumber, institutionCode = institutioncode)
class(colectores_inpa$catalogNumber)
#loop----
for (i in 1:length(especies)) {
    #nomes
    nome_arquivo <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "raw.csv")
    nome_out <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa.csv")
    print(paste("Inpa", especies[i], i, "de", length(especies), sep = " "))
    if(!file.exists(nome_out)) {
    #le a tabela
    tabela_especie <- read.csv(nome_arquivo, row.names = 1, stringsAsFactors = F) %>%
        mutate(catalogNumber = factor(catalogNumber))

    #junta a tabela com os coletores
    if ("Instituto Nacional de Pesquisas da Amazônia (INPA)" %in% tabela_especie$institutionCode) {
    proof_name <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa_proof.csv")
    tabela_especie <- left_join(tabela_especie, colectores_inpa)
    #tudo o que for do inpa bota o coletor e tira a coluna collector
    tabela_especie <- tabela_especie %>%
        mutate(recordedBy = if_else(tabela_especie$institutionCode %in%
                                        "Instituto Nacional de Pesquisas da Amazônia (INPA)", collector, recordedBy)) %>%
        dplyr::select(-collector)
    #escreve
    write.csv(tabela_especie, file = nome_out)
    proof <- tabela_especie %>%
        filter(institutionCode == "Instituto Nacional de Pesquisas da Amazônia (INPA)") %>%
        dplyr::select(institutionCode, catalogNumber, recordedBy) %>%
        distinct()
    write.csv(proof, file = proof_name)
    } else {
 write.csv(tabela_especie, file = nome_out)
    }
    }
}


#########################################
    #Limpeza de registros

for (i in 1:length(especies)) {
    print(paste("Limpando", especies[i], i, "de", length(especies), sep = " "))
    ###     o arquivo original sem o inpa
    nome_arquivo <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "raw.csv")
    ###     o arquivo com o inpa
    nome_inpa <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa.csv")
    ###     o nome do arquivo que será criado com registros excluídos
    nome_excluded <- paste0("./output_final/",familias[i],"/",familias[i], "_", especies[i],"_",
                            "excluded.csv")
### o nome do arquivo que será criado com registros duplicados (coletor, ano, numero de coleta). nao incluo nos excluded porque ele não vai ser excluido, vai ter uma cópia dele em clean e o duplicado não pode aparecer no excluded, vai ser confuso. mas eu quero checar que os s.n. não foram tomados como duplicados:
    nome_duplicata <- paste0("./output_final/",familias[i],"/",familias[i], "_", especies[i],"_",
                            "duplicata.csv")
### o nome do arquivo limpo que será criado no final
    nome_clean <- paste0("./output_final/",familias[i],"/",familias[i], "_", especies[i],"_",
                         "clean.csv")
    #if (!file.exists(nome_clean)) {

#agora a "tabela" é tabela_especie, entao tenho que substituir isso tudo
    #vai ser com a tabela do inpa
tabela_especie <- read.csv(nome_inpa, row.names = 1, stringsAsFactors = F)

#remover registros não informativos, sem coletor, numero de coleta, ano e informações de localidade----
#seleciona os registros que devem sair
tabela_exclude1 <- tabela_especie %>% dplyr::filter(is.na(year) &
                                                        is.na(recordedBy) &
                                                        is.na(stateProvince) &
                                                        is.na(municipality) &
                                                        is.na(locality))
#a tabela de especie menos esses registros (antijoin)
tabela_especie <- dplyr::anti_join(tabela_especie, tabela_exclude1)
#remover registros fora do brasil ou que não tem informação de país----
#seleciona os registros que devem sair
tabela_exclude2 <- tabela_especie %>% dplyr::filter(is.na(country) | country != "Brazil")
#a tabela de especie menos esses registros (antijoin)
tabela_especie <- dplyr::anti_join(tabela_especie, tabela_exclude2)
#registros que não tem nome de coletor nem numero de coleta
#seleciona os registros que devem sair
tabela_exclude3 <- tabela_especie %>%
    dplyr::filter(is.na(recordedBy) & is.na(recordNumber))
#a tabela de especie menos esses registros (antijoin)
tabela_especie <- dplyr::anti_join(tabela_especie, tabela_exclude3)
#junto todas as tabelas do que seria excluído (cada passo gerou tabelas diferentes)
tabela_exclude <- dplyr::bind_rows(tabela_exclude1, tabela_exclude2, tabela_exclude3)
write.csv(tabela_exclude, file = nome_excluded)
#esta excluded não inclui possíveis duplicados

#remover registros com número de coleta, estado e ano iguais (coletas colaborativas duplicadas)
#gera um vetor TrueFalse dizendo quem é duplicado, omitindo os s.n e NA
vetor_duplicata <- tabela_especie %>%
    dplyr::select(year, recordNumber, stateProvince) %>%
    new_duplicated(., incomparables = c("NA", "s.n", "s/n"))
#cria a tabela de duplicados e salva
tabela_duplicata <- tabela_especie[vetor_duplicata,]
#write.csv(tabela_duplicata, file = nome_duplicata)
#tira os duplicados da tabela especie
tabela_especie <- tabela_especie[!vetor_duplicata,]

####################################################
#registros com mesmo coletor e número de coleta
#preciso incluir escape s.n. e NA {next}
vetor_duplicata <- tabela_especie %>% dplyr::select(recordedBy, recordNumber) %>%
    new_duplicated(., incomparables = c("s.n", "s/n"))
#cria a tabela de duplicados e salva
tabela_duplicata <- bind_rows(tabela_duplicata, tabela_especie[vetor_duplicata,])
write.csv(tabela_duplicata, file = nome_duplicata)
#tira os duplicados da tabela especie
tabela_especie <- tabela_especie[!vetor_duplicata,]
write.csv(tabela_especie, file = nome_clean)
    #}
}




