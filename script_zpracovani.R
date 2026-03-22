library(dplyr)
setwd("C:/Users/N/Documents/Desktop/zatez_alt")

# Načtení dat
data_primar  <- read.csv("data_primar.csv", stringsAsFactors = FALSE)
data_ch      <- read.csv("data_ch.csv", stringsAsFactors = FALSE)
data_doplnek <- read.csv("data_doplnek.csv", stringsAsFactors = FALSE)

# --------------------------
# 1) Přizpůsobení data_ch tak, aby odpovídala data_primar
# --------------------------

# Přidání chybějících sloupců do data_ch
chybejici_sloupce <- setdiff(names(data_primar), names(data_ch))
for (sloupec in chybejici_sloupce) {
  data_ch[[sloupec]] <- NA
}

# Ponechání pouze sloupců, které jsou v data_primar,
# a ve stejném pořadí
data_ch_upravena <- data_ch[, names(data_primar), drop = FALSE]

# Spojení pod sebe
data_spojena <- rbind(data_primar, data_ch_upravena)

# --------------------------
# 2) Merge s doplňkem podle mailu
# --------------------------

data_final <- merge(
  data_spojena,
  data_doplnek,
  by.x = "E.mailová.adresa",
  by.y = "Váš.školní.e.mail",
  all.x = TRUE
)



for (i in 1:nrow(data_spojena)) {
  
  mail <- data_spojena$E.mailová.adresa[i]
  
  shoda <- data_doplnek$Váš.školní.e.mail == mail
  
  if (!any(shoda, na.rm = TRUE)) {
    print(data_spojena$Do.jaké.třídy.chodíte.[i])
  }
}


colnames(data_final)
colnames(data_final)[c(8:19)] <- paste0(colnames(data_final)[c(8:19)], " (SAK)")
colnames(data_final)[c(20:31)] <- paste0(colnames(data_final)[c(20:31)], " (PIV)")
colnames(data_final)[c(32:40)] <- paste0(colnames(data_final)[c(32:40)], " (MATEMATIKA)")
colnames(data_final)[c(41:50)] <- paste0(colnames(data_final)[c(41:50)], " (ANGLIČTINA)")
colnames(data_final)[c(51:59)] <- paste0(colnames(data_final)[c(51:59)], " (ČEŠTINA A KOMUNIKACE)")
colnames(data_final)[c(60:68)] <- paste0(colnames(data_final)[c(60:68)], " (DRUHÝ CIZÍ JAZYK)")
colnames(data_final)[c(69:77)] <- paste0(colnames(data_final)[c(69:77)], " (OSV)")
colnames(data_final)[c(78:86)] <- paste0(colnames(data_final)[c(78:86)], " (TVORBA)")
colnames(data_final)[c(89:94)] <- paste0(colnames(data_final)[c(89:94)], " (IKT)")
colnames(data_final)[c(96:101)] <- paste0(colnames(data_final)[c(96:101)], " (LITERATURA A UMĚNÍ)")

colnames(data_final)
View(data_final)

data_fin <- data_final %>% select(-c(1, 2, 87))
write.csv(data_fin, file = "data_pretizeni.csv")
write.csv(data_fin[c(1:50),], file = "data_pretizenik.csv")











