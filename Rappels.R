log_ex <- read.csv("L:/BUT/SD/Promo 2023/tcessac/2ème année/R Shiny/1. Rappels/dpe-v2-logements-existants.csv", header = TRUE, sep = ",")
log_neuf <- read.csv("L:/BUT/SD/Promo 2023/tcessac/2ème année/R Shiny/1. Rappels/dpe-v2-logements-neufs.csv", header = TRUE, sep = ",")

dim(log_ex)
dim(log_neuf)

log_ex$Logement <- "ancien"
log_neuf$Logement <- "neuf"

log_neuf$Année_construction <- 2024

col_communes <- intersect(names(log_ex),names(log_neuf))
log_ex2 <- log_ex[,col_communes]
log_neuf2 <- log_neuf[,col_communes]
tous_log <- rbind(log_ex2,log_neuf2)

tous_log$Année <- substr(tous_log$Date_réception_DPE, 0, 4)
tous_log$Année_construction = as.numeric(tous_log$Année_construction)


for (i in 1:nrow(tous_log)){
  if(tous_log$Coût_total_5_usages[i] == tous_log$Coût_chauffage[i] + tous_log$Coût_éclairage[i] + tous_log$Coût_ECS[i] + tous_log$Coût_refroidissement[i] + tous_log$Coût_auxiliaires[i]){
    tous_log$Vérif[i] <- "OK"
  }else{
    tous_log$Vérif[i] <- "Pas OK"
  }
  if(tous_log$Coût_total_5_usages[i] != 0){
    tous_log$Coût_chauffage_pourcent[i] <- (tous_log$Coût_chauffage[i] / tous_log$Coût_total_5_usages[i]) * 100
  } else {
    tous_log$Coût_chauffage_pourcent[i] <- NA  
  }
}

tous_log$Periode_construction <- cut(
  tous_log$Année_construction,
  breaks = c(-Inf, 1960, 1970, 1980, 1990, 2000, 2010, Inf),
  labels = c("Avant 1960", "1961 - 1970", "1971 - 1980", "1981 - 1990", "1991 - 2000", "2001 - 2010", "Après 2010"),
  right = TRUE
)

table(tous_log$Etiquette_DPE)
table(tous_log$Année_construction)
mean(tous_log$Surface_habitable_logement, na.rm = TRUE)
quantile(tous_log$Coût_ECS, probs = seq(0,1,0.25), na.rm = TRUE)
cor(x = tous_log$Surface_habitable_logement, y = tous_log$Coût_chauffage, use = "complete.obs")

install.packages("corrplot")
require("corrplot")

corr_log = tous_log[,c("Coût_total_5_usages","Coût_chauffage","Coût_éclairage","Coût_ECS","Coût_refroidissement", "Coût_auxiliaires", "Surface_habitable_logement" , "Emission_GES_5_usages")]
corrplot(corr = cor(corr_log, use = "complete.obs"), type = "lower")

df1 = subset(tous_log, tous_log$Type_bâtiment == "appartement")
df2 = subset(tous_log, tous_log$Etiquette_DPE == c("D","E","F","G"))
df3 = subset(tous_log, tous_log$Année_construction < 1948)
df4 = subset(tous_log, tous_log$Surface_habitable_logement > mean(tous_log$Surface_habitable_logement))
