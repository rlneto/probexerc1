# Inicializando

library(data.table)
base <- fread(input = paste0("mobile.csv"), header = T, na.strings = "NA", data.table = FALSE, dec = ".")
library(dplyr)

# Item 2

base$price_range <- recode(base$price_range,`0`="baixo",`1`="médio",`2`="caro", `3`="muito caro")
freq.tabela <- table(base$price_range, useNA = "ifany")
freq.tabela
porc.tabela <- round(prop.table(freq.tabela)*100,1)
porc.tabela
freq.tabela <- data.frame(freq.tabela,porc.tabela)
freq.tabela <- freq.tabela[,-3]
colnames(freq.tabela) <- c("Preço","Frequencia","Porcentagem")
png(file = "setores1.png")
pie(freq.tabela$Frequencia,freq.tabela$Preço,
    main="Faixa de preço")
dev.off()

# Item 3
analise2 <- base[,c("blue", "wifi")]
analise2$blue <- as.character(analise2$blue)
analise2$wifi <- as.character(analise2$wifi)
library(dplyr)
analise2$blue <- recode(analise2$blue,`0`="Não",`1`="Sim")
analise2$wifi <- recode(analise2$wifi,`0`="Não",`1`="Sim")
colnames(analise2) <- c("Bluetooth", "Wi-fi")
tabela2 <- data.frame(table(analise2$Bluetooth, analise2$`Wi-fi`))
colnames(tabela2) <- c("Bluetooth", "Wi-fi", "Ocorrências")
rownames(tabela2) <- c("Nenhum dos dois", "Somente Bluetooth", "Somente Wi-fi", "Possui ambos")
library(ggplot2)
png(file="barras2.png")
barplot(height=tabela2$Ocorrências,
        names=c("Nenhum dos dois", "Somente BT", "Somente Wi-fi", "Possui ambos"),
        col=rgb(0.3,0.6,0.5,0.5),
        xlab="BT/Wi-fi", 
        ylab="Ocorrências", 
        main="")
dev.off()
# Item 4
analise3 <- base[,c("mobile_wt", "ram")]
colnames(analise3) <- c("peso", "ram")
library(ggplot2)
png(file="dispersao4.png")
ggplot(data=analise3, aes(peso , ram)) +
  geom_point()
dev.off()

# Item 5

prof_media <- round(mean(base$m_dep), 2)
freq5.tabela <- table(base$m_dep, useNA = "ifany")
freq5.tabela
png(file = "barras5.png")
barplot(height=freq5.tabela, names=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
        col=rgb(0.3,0.6,0.5,0.5),
        xlab="Profundidade (mm)", 
        ylab="Ocorrências", 
        main="")
dev.off()

# Item 6

base$memoria[base$ram <1000]  = "Menor que 1GB"
base$memoria[base$ram >=1000 & base$ram <2000]  = "Entre 1 e 2 GB de RAM"
base$memoria[base$ram >=2000 & base$ram <4000]  = "Entre 2 e 4 GB de RAM"
base$memoria[base$ram >=4000]  = "Mais que 4GB de RAM"

freq6.tabela <- table(base$memoria, useNA = "ifany") 
freq6.tabela
