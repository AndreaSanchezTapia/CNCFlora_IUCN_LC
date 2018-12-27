#join everything
#aoo <- read.csv("./results/final_with_aooeoo.csv", row.names = 1)
#aoo <- read.csv("./results/aoo_veg.csv", row.names = 1)
aoo <- read.csv("./results/aoo_veg_cites.csv", row.names = 1)
use <- read.csv("./results/Use_results_general.csv", row.names = 1)
ucs <- read.csv("./results/tabla_UCs.csv", row.names = 1)
names(aoo)
names(use)
names(ucs)
use <- use %>% rename(nombre = especies)
final <- left_join(aoo, use) %>% left_join(ucs)
View(final)

write.csv(final, "./results/final_results.csv")
