# ---- Pedro Milreu Cunha - Mestrando em Economia Aplicada - PPGE/UFPB ---- #

library(data.table) #Biblioteca para leitura dos dados


#-------------------------------------------------------------------------#

#### 1) FEDERAIS X ESTADUAIS ####

#-------------------------------------------------------------------------#

#### 1.1) Dados 2009 ####

dados_2009 <- fread("Dados/MICRODADOS_ENADE_2009.txt", sep = "\t",
                    select = c(1:3, 5, 9:12, 21, 28, 60, 74:75, 78, 87))

dados_2009 <- na.omit(dados_2009)
dados_2009 <- dados_2009[!grepl("_", dados_2009$QE_I1, fixed = TRUE),]
dados_2009 <- dados_2009[!grepl("_", dados_2009$QE_I2, fixed = TRUE),]
dados_2009 <- dados_2009[!grepl("_", dados_2009$QE_I5, fixed = TRUE),]
dados_2009 <- dados_2009[!grepl("_", dados_2009$QE_I14, fixed = TRUE),]

#### Deixando apenas as faculdades federais e estaduais ####

dados_2009 <- subset(dados_2009,CO_CATEGAD %in% c(1,2))

#### Recodificação das variáveis ####

dados_2009$NU_IDADE2 <- dados_2009$NU_IDADE**2

dados_2009$Tratado <- 0

dados_2009$Sexo <- ifelse(dados_2009$TP_SEXO == 2, 0, 1)

dados_2009$Valido <- ifelse(dados_2009$AMOSTRA == 1 & dados_2009$TP_PRES == 555,
                            1, 0)

dados_2009$Casado <- ifelse(dados_2009$QE_I1 == "B", 1, 0)

dados_2009$Branco <- ifelse(dados_2009$QE_I2 == "A", 1, 0)

dados_2009$Renda_3SM <- ifelse(dados_2009$QE_I5 %in% c("A","B"), 1, 0)

dados_2009$Medio_ou_mais <- ifelse(dados_2009$QE_I14 %in% c("D","E","F"), 1, 0)

#### Deixando apenas as observações válidas ####

dados_2009 <- subset(dados_2009, Valido == 1)

dados_2009 <- na.omit(dados_2009)

#### Base final ####

dados_2009_final <- dados_2009[, -c(3,8:10,12:15, 19)]
dados_2009_final <- subset(dados_2009_final, Sexo %in% c(0,1))

#-------------------------------------------------------------------------#

#-------------------------------------------------------------------------#

#### 1.2) Dados 2010 ####

dados_2010 <- fread("Dados/MICRODADOS_ENADE_2010.txt", sep = ";",
                    select = c(1:3, 5:8, 11:12, 20, 30, 41, 62:63,66,75))

dados_2010 <- na.omit(dados_2010)
dados_2010 <- dados_2010[!grepl("_", dados_2010$QE_I01, fixed = TRUE),]
dados_2010 <- dados_2010[!grepl("_", dados_2010$QE_I02, fixed = TRUE),]
dados_2010 <- dados_2010[!grepl("_", dados_2010$QE_I05, fixed = TRUE),]
dados_2010 <- dados_2010[!grepl("_", dados_2010$QE_I14, fixed = TRUE),]

#### Deixando apenas as faculdades federais e estaduais ####

dados_2010 <- subset(dados_2010,CO_CATEGAD %in% c(10001,10002))

#### Recodificação das variáveis ####

dados_2010$NU_IDADE2 <- dados_2010$NU_IDADE**2

dados_2010$Tratado <- 0

dados_2010$Sexo <- ifelse(dados_2010$TP_SEXO == "M", 1,
                          ifelse(dados_2010$TP_SEXO == "F", 0, 100))

dados_2010$Valido <- ifelse(dados_2010$AMOSTRA == 1 & dados_2010$TP_PRES == 555,
                            1, 0)

dados_2010$Casado <- ifelse(dados_2010$QE_I01 == "B", 1, 0)

dados_2010$Branco <- ifelse(dados_2010$QE_I02 == "A", 1, 0)

dados_2010$Renda_3SM <- ifelse(dados_2010$QE_I05 %in% c("A","B"), 1, 0)

dados_2010$Medio_ou_mais <- ifelse(dados_2010$QE_I14 %in% c("D","E","F"), 1, 0)

#### Deixando apenas as observações válidas ####

dados_2010 <- subset(dados_2010, Valido == 1)

dados_2010 <- na.omit(dados_2010)

#### Base final ####

dados_2010_final <- dados_2010[, -c(3,6,9:11,13:16, 20)]
dados_2010_final <- subset(dados_2010_final, Sexo %in% c(0,1))

#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#

#### 1.3) Dados 2011 ####

dados_2011 <- fread("Dados/MICRODADOS_ENADE_2011.txt", sep = ";",
                    select = c(1:3, 5:8, 11:12, 20, 30, 41, 62:63,66,75))

dados_2011 <- na.omit(dados_2011)
dados_2011 <- dados_2011[!grepl("_", dados_2011$QE_I01, fixed = TRUE),]
dados_2011 <- dados_2011[!grepl("_", dados_2011$QE_I02, fixed = TRUE),]
dados_2011 <- dados_2011[!grepl("_", dados_2011$QE_I05, fixed = TRUE),]
dados_2011 <- dados_2011[!grepl("_", dados_2011$QE_I14, fixed = TRUE),]

#### Deixando apenas as faculdades federais e estaduais ####

dados_2011 <- subset(dados_2011,CO_CATEGAD %in% c(10001,10002))

#### Recodificação das variáveis ####

dados_2011$NU_IDADE2 <- dados_2011$NU_IDADE**2

dados_2011$Tratado <- 0

dados_2011$Sexo <- ifelse(dados_2011$TP_SEXO == "M", 1,
                          ifelse(dados_2011$TP_SEXO == "F", 0, 100))

dados_2011$Valido <- ifelse(dados_2011$AMOSTRA == 1 & dados_2011$TP_PRES == 555,
                            1, 0)

dados_2011$Casado <- ifelse(dados_2011$QE_I01 == "B", 1, 0)

dados_2011$Branco <- ifelse(dados_2011$QE_I02 == "A", 1, 0)

dados_2011$Renda_3SM <- ifelse(dados_2011$QE_I05 %in% c("A","B"), 1, 0)

dados_2011$Medio_ou_mais <- ifelse(dados_2011$QE_I14 %in% c("D","E","F"), 1, 0)

#### Deixando apenas as observações válidas ####

dados_2011 <- subset(dados_2011, Valido == 1)

dados_2011 <- na.omit(dados_2011)

#### Base final ####

dados_2011_final <- dados_2011[, -c(3,6,9:11,13:16, 20)]
dados_2011_final <- subset(dados_2011_final, Sexo %in% c(0,1))

#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#

#### 1.4) Dados 2012 ####

dados_2012 <- fread("Dados/MICRODADOS_ENADE_2012.txt", sep = ",",
                    select = c(1:3, 5:8, 11:12, 20, 30, 41, 62:63, 66, 75))

dados_2012 <- na.omit(dados_2012)
dados_2012 <- dados_2012[!grepl("_", dados_2012$QE_I01, fixed = TRUE),]
dados_2012 <- dados_2012[!grepl("_", dados_2012$QE_I02, fixed = TRUE),]
dados_2012 <- dados_2012[!grepl("_", dados_2012$QE_I05, fixed = TRUE),]
dados_2012 <- dados_2012[!grepl("_", dados_2012$QE_I14, fixed = TRUE),]

#### Deixando apenas as faculdades federais e estaduais ####

dados_2012 <- subset(dados_2012,CO_CATEGAD %in% c(10001,10002))

#### Recodificação das variáveis ####

dados_2012$NU_IDADE2 <- dados_2012$NU_IDADE**2

dados_2012$Tratado <- ifelse(dados_2012$CO_CATEGAD == 10002, 1, 0)

dados_2012$Sexo <- ifelse(dados_2012$TP_SEXO == "M", 1,
                          ifelse(dados_2012$TP_SEXO == "F", 0, 100))

dados_2012$Valido <- ifelse(dados_2012$AMOSTRA == 1 & dados_2012$TP_PRES == 555,
                            1, 0)

dados_2012$Casado <- ifelse(dados_2012$QE_I01 == "B", 1, 0)

dados_2012$Branco <- ifelse(dados_2012$QE_I02 == "A", 1, 0)

dados_2012$Renda_3SM <- ifelse(dados_2012$QE_I05 %in% c("A","B","C"), 1, 0)

dados_2012$Medio_ou_mais <- ifelse(dados_2012$QE_I14 %in% c("D","E","F"), 1, 0)

#### Deixando apenas as observações válidas ####

dados_2012 <- subset(dados_2012, Valido == 1)

dados_2012 <- na.omit(dados_2012)

#### Base final ####

dados_2012_final <- dados_2012[, -c(3,6,9:11,13:16, 20)]
dados_2012_final <- subset(dados_2012_final, Sexo %in% c(0,1))

#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#

#### 1.5) Dados 2013 ####

dados_2013 <- fread("Dados/MICRODADOS_ENADE_2013.txt", sep = ";",
                    select = c(1:3, 5:8, 11:12, 20, 30, 41, 66:67, 70, 72))

dados_2013 <- na.omit(dados_2013)
dados_2013 <- dados_2013[!grepl("_", dados_2013$QE_I01, fixed = TRUE),]
dados_2013 <- dados_2013[!grepl("_", dados_2013$QE_I02, fixed = TRUE),]
dados_2013 <- dados_2013[!grepl("_", dados_2013$QE_I05, fixed = TRUE),]
dados_2013 <- dados_2013[!grepl("_", dados_2013$QE_I07, fixed = TRUE),]

#### Deixando apenas as faculdades federais e estaduais ####

dados_2013 <- subset(dados_2013,CO_CATEGAD %in% c(93,10001,10002))

#### Recodificação das variáveis ####

dados_2013$NU_IDADE2 <- dados_2013$NU_IDADE**2

dados_2013$Tratado <- 0

dados_2013$Sexo <- ifelse(dados_2013$TP_SEXO == "M", 1,
                          ifelse(dados_2013$TP_SEXO == "F", 0, 100))

dados_2013$Valido <- ifelse(dados_2013$AMOSTRA == 1 & dados_2013$TP_PRES == 555,
                            1, 0)

dados_2013$Casado <- ifelse(dados_2013$QE_I01 == "B", 1, 0)

dados_2013$Branco <- ifelse(dados_2013$QE_I02 == "A", 1, 0)

dados_2013$Renda_3SM <- ifelse(dados_2013$QE_I07 %in% c("A","B","C"), 1, 0)

dados_2013$Medio_ou_mais <- ifelse(dados_2013$QE_I05 %in% c("D","E","F"), 1, 0)

#### Deixando apenas as observações válidas ####

dados_2013 <- subset(dados_2013, Valido == 1)

dados_2013 <- na.omit(dados_2013)

#### Base final ####

dados_2013_final <- dados_2013[, -c(3,6,9:11,13:16, 20)]
dados_2013_final <- subset(dados_2013_final, Sexo %in% c(0,1))

#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#

#### 1.6) Dados 2014 ####

dados_2014 <- fread("Dados/MICRODADOS_ENADE_2014.txt", sep = ";",
                    select = c(1:3, 5:8, 11:12, 20, 38, 49, 74:75, 78, 81))

dados_2014 <- na.omit(dados_2014)
dados_2014 <- dados_2014[!grepl("_", dados_2014$QE_I01, fixed = TRUE),]
dados_2014 <- dados_2014[!grepl("_", dados_2014$QE_I02, fixed = TRUE),]
dados_2014 <- dados_2014[!grepl("_", dados_2014$QE_I05, fixed = TRUE),]
dados_2014 <- dados_2014[!grepl("_", dados_2014$QE_I08, fixed = TRUE),]


#### Deixando apenas as faculdades federais e estaduais ####

dados_2014 <- subset(dados_2014,CO_CATEGAD %in% c(93,10001,10002))

#### Recodificação das variáveis ####

dados_2014$NU_IDADE2 <- dados_2014$NU_IDADE**2

dados_2014$Tratado <- 0

dados_2014$Sexo <- ifelse(dados_2014$TP_SEXO == "M", 1,
                          ifelse(dados_2014$TP_SEXO == "F", 0, 100))

dados_2014$Valido <- ifelse(dados_2014$AMOSTRA == 1 & dados_2014$TP_PRES == 555,
                            1, 0)

dados_2014$Casado <- ifelse(dados_2014$QE_I01 == "B", 1, 0)

dados_2014$Branco <- ifelse(dados_2014$QE_I02 == "A", 1, 0)

dados_2014$Renda_3SM <- ifelse(dados_2014$QE_I08 %in% c("A","B"), 1, 0)

dados_2014$Medio_ou_mais <- ifelse(dados_2014$QE_I05 %in% c("D","E","F"), 1, 0)

#### Deixando apenas as observações válidas ####

dados_2014 <- subset(dados_2014, Valido == 1)

dados_2014 <- na.omit(dados_2014)

#### Base final ####

dados_2014_final <- dados_2014[, -c(3,6,9:11,13:16, 20)]
dados_2014_final <- subset(dados_2014_final, Sexo %in% c(0,1))

#-------------------------------------------------------------------------#

#-------------------------------------------------------------------------#

#### 1.7) Unindo os dados ####

final_federal_estadual <- rbind(dados_2009_final,dados_2010_final,
                                dados_2011_final,dados_2012_final,
                                dados_2013_final,dados_2014_final)

final_federal_estadual <- subset(final_federal_estadual, Sexo != 100)

#### Criação de uma dummy indicando se a IES foi tratada ou não ####

tratadas_temp <- subset(final_federal_estadual, Tratado == 1)
tratadas_temp <- tratadas_temp$CO_IES


Tratadas <- ifelse(final_federal_estadual$CO_IES %in%
                     c(tratadas_temp), 1, 0)

final_federal_estadual$Tratadas <- Tratadas
#-------------------------------------------------------------------------#

#-------------------------------------------------------------------------#

#### 1.8) Exportando as bases ####

#write.csv(dados_2009_final, "Dados trabalhados/dados_2009_final.csv", row.names = TRUE)
#write.csv(dados_2010_final, "Dados trabalhados/dados_2010_final.csv", row.names = TRUE)
#write.csv(dados_2011_final, "Dados trabalhados/dados_2011_final.csv", row.names = TRUE)
#write.csv(dados_2012_final, "Dados trabalhados/dados_2012_final.csv", row.names = TRUE)
#write.csv(dados_2013_final, "Dados trabalhados/dados_2013_final.csv", row.names = TRUE)
#write.csv(dados_2014_final, "Dados trabalhados/dados_2014_final.csv", row.names = TRUE)
#write.csv(final_federal_estadual, "Dados trabalhados/final_federal_estadual.csv", row.names = TRUE)

saveRDS(dados_2009_final, "Dados trabalhados/dados_2009_final.rds")
saveRDS(dados_2010_final, "Dados trabalhados/dados_2010_final.rds")
saveRDS(dados_2011_final, "Dados trabalhados/dados_2011_final.rds")
saveRDS(dados_2012_final, "Dados trabalhados/dados_2012_final.rds")
saveRDS(dados_2013_final, "Dados trabalhados/dados_2013_final.rds")
saveRDS(dados_2014_final, "Dados trabalhados/dados_2014_final.rds")
saveRDS(final_federal_estadual, "Dados trabalhados/final_federal_estadual.rds")
