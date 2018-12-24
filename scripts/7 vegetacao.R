#vegetacao
finalaoo <- read.csv("./results/final_with_aooeoo.csv", row.names = 1)
names(finalaoo)
unique(finalaoo$vegetationType)
veg_comp <- readxl::read_excel("./data/Tabela comparativa - Tipos de vegetação IUCN e Flora do Brasil-11-12-2018.xlsx") %>% data.frame()
names(veg_comp) <- c("IUCN", "CNCFlora", "FB")
head(veg_comp)
veg_comp %>% count(FB,CNCFlora) %>% View()
unique(veg_comp$FB)

w <- grepl(pattern = veg_comp$FB[1], x = finalaoo$vegetationType)
finalaoo$vegetationType[w]

i <- 1
aoo_veg <- finalaoo
for (i in seq_along(unique(veg_comp$FB))) {
    vegname <- unique(veg_comp$FB)[i]
    data <- veg_comp %>% filter(FB == vegname)
    print(data)
    gr <- grep(pattern = vegname, x = finalaoo$vegetationType)
    aoo_veg[[vegname]]  <- ""
    aoo_veg[[vegname]][gr]  <- data$CNCFlora
}
names(aoo_veg)
View(aoo_veg)

aoo_veg2 <- aoo_veg %>% tidyr::unite(IUCN, 23:70, sep =" ")
View(aoo_veg2)

write.csv(aoo_veg2, "./results/aoo_veg.csv")
