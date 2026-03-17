
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







