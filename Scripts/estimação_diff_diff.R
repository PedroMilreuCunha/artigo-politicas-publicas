# ---- Pedro Milreu Cunha - Mestrando em Economia Aplicada - PPGE/UFPB ---- #

#### Bibliotecas ####

library(dplyr)
library(plm)
library(cobalt)
library(WeightIt)
library(ebal)
library(stargazer)
library(ggplot2)
library(lmtest)
library(MatchIt)
library(readxl)

#### Leitura dos dados ####

painel_federal_estadual <- readRDS("Dados trabalhados/painel_federal_estadual.rds")
painel_federal_estadual$Tratamento <- as.factor(painel_federal_estadual$Tratamento)

#### Gráfico de densidade das notas médias ####

grafico_densidade_notas <- ggplot(painel_federal_estadual, aes(x = nota_media, fill = Tratamento))+
  geom_density(alpha = 0.1) +
  labs(x = "Nota média", y = "Densidade", fill = "Grupo")+
  theme_minimal()

grafico_densidade_notas <- grafico_densidade_notas + 
  scale_fill_discrete(labels=c("Controle", "Tratamento"))

#ggsave(filename = "Figuras/grafico_densidade_notas.png")

#### Criando o logaritmo da nota para suavizar a assimetria da distribuição ####

painel_federal_estadual$log_nota_media <- log(painel_federal_estadual$nota_media)

#### Gráfico de densidade do logaritmo das notas médias ####

grafico_densidade_log_notas <- ggplot(painel_federal_estadual, aes(x = log_nota_media, fill = Tratamento))+
  geom_density(alpha = 0.1) +
  labs(x = "Logaritmo natural da nota média", y = "Densidade", fill = "Grupo")+
  theme_minimal()

grafico_densidade_log_notas <- grafico_densidade_log_notas + 
  scale_fill_discrete(labels=c("Controle", "Tratamento"))

#ggsave(filename = "Figuras/grafico_densidade_log_notas.png")

#### Nomes das variáveis ####

names <- data.frame(old = c("idade_media", "idade2_media", "prop_brancos",
                            "prop_casados", "prop_homens", "prop_medio_ou_mais",
                            "prop_renda_3SM"),
                    new = c("Idade Média", "Idade Média²",
                            "Proporção de brancos", "Proporção de casados",
                            "Proporção de homens", "Proporção de mães com ensino médio ou mais",
                            "Proporção de alunos com renda familiar inferior a 3 s.m.."))

names_placebo <- data.frame(old = c("idade_media", "idade2_media", "prop_brancos",
                                    "nota_media", "prop_homens", "prop_medio_ou_mais",
                                    "prop_renda_3SM"),
                            new = c("Idade Média", "Idade Média²",
                                    "Proporção de brancos", "Nota média",
                                    "Proporção de homens", "Proporção de mães com ensino médio ou mais",
                                    "Proporção de alunos com renda familiar inferior a 3 s.m.."))


#### Hipótese das tendências paralelas ####

tendencias <- ggplot(painel_federal_estadual, aes(Ano, nota_media, color = Tratamento))+
  stat_summary(geom = 'line') +
  labs(y = "Nota média", color = "Grupo")+
  geom_vline(xintercept = 2012) +
  scale_colour_discrete(labels=c("Controle", "Tratamento"))+
  theme_minimal()

ggsave(file = "Figuras/tendencias.png")

#### **Ciclo 2009-2012** ####

painel_federal_estadual_2009_2012 <- subset(painel_federal_estadual,
                                            Ano %in% c(2009, 2012))

painel_federal_estadual_2009_2012$log_nota <- log(painel_federal_estadual_2009_2012$nota_media)

federal_estadual_2009 <- subset(painel_federal_estadual_2009_2012, Ano == 2009)

#### Hipótese do suporte comum ####

ps <- glm(data = painel_federal_estadual_2009_2012,
          Tratamento ~ idade_media + idade2_media + prop_brancos +
            prop_casados + prop_homens + prop_medio_ou_mais + 
            prop_renda_3SM, family = binomial(link = "logit"))

painel_federal_estadual_2009_2012$pscore <- predict(ps, type = "response")

grafico_suporte_comum_2009_2012 <- ggplot(painel_federal_estadual_2009_2012,
                                          aes(x = pscore, fill = Tratamento))+
  geom_density(alpha = 0.4) +
  labs(x = "Propensity score",
       y = "Densidade", 
       fill = "Grupo")+
  theme_minimal()

grafico_suporte_comum_2009_2012 <- grafico_suporte_comum_2009_2012 + 
  scale_fill_discrete(labels=c("Controle",
                               "Tratamento"))

ggsave(file = "Figuras/suporte_comum_2009_2012.png")

## Suporte comum por quantil 

painel_federal_estadual_2009_2012$quantil = with(painel_federal_estadual_2009_2012,
                                                 cut(pscore,
                                                     quantile(pscore,
                                                              prob = seq(0, 0.984, length = 5),
                                                              type = 5)))

media_por_quantil_2009_2012 <- subset(painel_federal_estadual_2009_2012, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))

grafico_suporte_comum_2009_2012_quantil <- ggplot(subset(painel_federal_estadual_2009_2012, !is.na(quantil)),
                                                  aes(x = pscore, fill = Tratamento,
                                                      color = quantil))+
  geom_density(alpha = 0.4) +
  labs(x = "Propensity score",
       y = "Densidade", 
       fill = "Grupo",
       colour = "Quantil")+
  theme_minimal()

grafico_suporte_comum_2009_2012_quantil <- grafico_suporte_comum_2009_2012_quantil + 
  scale_fill_discrete(labels=c("Controle",
                               "Tratamento"))
grafico_suporte_comum_2009_2012_quantil + facet_grid(quantil ~ .)

ggsave(file = "Figuras/suporte_comum_2009_2012_quantis.png")

#### Modelo diff-diff simples ####

didreg = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
              idade2_media + prop_brancos + prop_casados + prop_homens + 
              prop_medio_ou_mais + prop_renda_3SM,
            data = painel_federal_estadual_2009_2012)

names(didreg$coefficients) <- c("Constante", "Tratamento", "Pós-tratamento",
                                "Idade média", "Idade média²", "Prop. brancos", 
                                "Prop. casados", "Prop. homens", "Prop. Ensino Médio+",
                                "Prop. Renda até 3SM","Diff-and-diff/Impacto")

summary(didreg)

#### Modelo diff-and-diff com efeitos fixos ####

didreg_fe <- plm(log_nota ~ Tempo + Tempo*Tratamento + idade_media +
                   idade2_media + prop_brancos + prop_casados + prop_homens + 
                   prop_medio_ou_mais + prop_renda_3SM,
                 model = "within", index = c("CO_IES", "Ano"),
                 data = painel_federal_estadual_2009_2012)

names(didreg_fe$coefficients) <- c("Pós-tratamento",
                                   "Idade média", "Idade média²", "Prop. brancos", 
                                   "Prop. casados", "Prop. homens", "Prop. Ensino Médio+",
                                   "Prop. Renda até 3SM","Diff-and-diff/Impacto")

#### Modelo diff-and-diff com efeitos fixos e PSM ####

#---- 1. Criação dos pesos utilizando o modelo logit multinomial #----

W1 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2009,
              method = "ps",
              estimand = "ATT")

#==== 1.1) Checagem do balanceamento #====

b1 = bal.tab(W1, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b1)

love.plot(W1, stats = c("m", "v"), drop.distance = TRUE,
          thresholds = c(m=0.1, v = 0.05),
          wrap = 10,
          var.order = "unadjusted",
          sample.names = c("Não pareado", "Pareado"),
          title = NULL,
          subtitle = "Amostra", line= T,
          var.names = names,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"))

#---- 2. Criação dos pesos utilizando o modelo de entropia #----

W2 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2009,
              method = "ebal",
              estimand = "ATT")

W2_placebo <- weightit(Tratamento ~ idade_media + idade2_media + 
                         prop_brancos + nota_media + prop_homens + 
                         prop_medio_ou_mais + prop_renda_3SM,
                       data = federal_estadual_2009,
                       method = "ebal",
                       estimand = "ATT")

W2_placebo_2 <- weightit(Tratamento ~ idade_media + idade2_media + 
                           prop_brancos + prop_casados + prop_homens + 
                           nota_media + prop_renda_3SM,
                         data = federal_estadual_2009,
                         method = "ebal",
                         estimand = "ATT")


#==== 2.1) Checagem do balanceamento #====

b2 = bal.tab(W2, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b2)

love.plot(W2, stats = c("m", "v"), drop.distance = TRUE,
          thresholds = c(m=0.1, v = 0.05),
          wrap = 10,
          var.order = "unadjusted",
          sample.names = c("Não pareado", "Pareado"),
          title = NULL,
          subtitle = "Amostra", line= T,
          var.names = names,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"))

b2_placebo = bal.tab(W2_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b2_placebo)

love.plot(W2_placebo, stats = c("m", "v"), drop.distance = TRUE,
          thresholds = c(m=0.1, v = 0.05),
          wrap = 10,
          var.order = "unadjusted",
          sample.names = c("Não pareado", "Pareado"),
          title = NULL,
          subtitle = "Amostra", line= T,
          var.names = names_placebo,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"))

b2_placebo_2 = bal.tab(W2_placebo_2, un = TRUE,
                       m.threshold=0.1,
                       v.threshold=3)
print(b2_placebo_2)

love.plot(W2_placebo_2, stats = c("m", "v"), drop.distance = TRUE,
          thresholds = c(m=0.1, v = 0.05),
          wrap = 10,
          var.order = "unadjusted",
          sample.names = c("Não pareado", "Pareado"),
          title = NULL,
          subtitle = "Amostra", line= T,
          var.names = names_placebo,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"))


#### Unindo os pesos ao data frame ####

W1_matrix <- data.frame(CO_IES = federal_estadual_2009$CO_IES,
                        W1 = W1$weights)

painel_federal_estadual_2009_2012_logit <- left_join(painel_federal_estadual_2009_2012,
                                                     W1_matrix, by = "CO_IES")

W2_matrix <- data.frame(CO_IES = federal_estadual_2009$CO_IES,
                        W2 = W2$weights)

painel_federal_estadual_2009_2012_entropia <- left_join(painel_federal_estadual_2009_2012,
                                                        W2_matrix, by = "CO_IES")

W2_placebo_matrix <- data.frame(CO_IES = federal_estadual_2009$CO_IES,
                                W2_placebo = W2_placebo$weights)

painel_federal_estadual_2009_2012_entropia_placebo <- left_join(painel_federal_estadual_2009_2012,
                                                                W2_placebo_matrix, by = "CO_IES")

W2_placebo_2_matrix <- data.frame(CO_IES = federal_estadual_2009$CO_IES,
                                  W2_placebo_2 = W2_placebo_2$weights)

painel_federal_estadual_2009_2012_entropia_placebo_2 <- left_join(painel_federal_estadual_2009_2012,
                                                                  W2_placebo_2_matrix, by = "CO_IES")


## Pesos calculados utilizando logit multinomial

didreg_fe_logit <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                         idade2_media + prop_brancos + prop_casados + prop_homens + 
                         prop_medio_ou_mais + prop_renda_3SM,
                       model = "within", index = c("CO_IES", "Ano"), weights = W1,
                       data = painel_federal_estadual_2009_2012_logit)

names(didreg_fe_logit$coefficients) <- c("Pós-tratamento",
                                         "Idade média", "Idade média²", "Prop. brancos", 
                                         "Prop. casados", "Prop. homens", "Prop. Ensino Médio+",
                                         "Prop. Renda até 3SM","Diff-and-diff/Impacto")

## Pesos calculados utilizando entropia

didreg_fe_entropia <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                            idade2_media + prop_brancos + prop_casados + prop_homens + 
                            prop_medio_ou_mais + prop_renda_3SM,
                          model = "within", index = c("CO_IES", "Ano"), weights = W2,
                          data = painel_federal_estadual_2009_2012_entropia)

names(didreg_fe_entropia$coefficients) <- c("Pós-tratamento",
                                            "Idade média", "Idade média²", "Prop. brancos", 
                                            "Prop. casados", "Prop. homens", "Prop. Ensino Médio+",
                                            "Prop. Renda até 3SM","Diff-and-diff/Impacto")

#### Efeitos marginais ####

coeficientes <- as.numeric(didreg_fe_entropia$coefficients)
efeitos_marginais <- data.frame(ID = rep(NaN, length(fitted.values(didreg_fe_entropia))),
                                Efeito = rep(NaN, length(fitted.values(didreg_fe_entropia))))

for (i in 1:170){
  
  y_temp <- with(painel_federal_estadual_2009_2012_entropia,
                 exp(coeficientes[1]*as.numeric(Tempo[i]) + 
                       coeficientes[2]*idade_media[i] + 
                       coeficientes[3]*idade2_media[i] + 
                       coeficientes[4]*prop_brancos[i] +
                       coeficientes[5]*prop_casados[i] +
                       coeficientes[6]*prop_homens[i] + 
                       coeficientes[7]*prop_medio_ou_mais[i] + 
                       coeficientes[8]*prop_renda_3SM[i] +
                       coeficientes[9]*1*as.numeric(Tempo[i])))
  y_base <- with(painel_federal_estadual_2009_2012_entropia,
                 exp(coeficientes[1]*as.numeric(Tempo[i]) + 
                       coeficientes[2]*idade_media[i] + 
                       coeficientes[3]*idade2_media[i] + 
                       coeficientes[4]*prop_brancos[i] +
                       coeficientes[5]*prop_casados[i] +
                       coeficientes[6]*prop_homens[i] + 
                       coeficientes[7]*prop_medio_ou_mais[i] + 
                       coeficientes[8]*prop_renda_3SM[i] +
                       coeficientes[9]*0*as.numeric(Tempo[i])))
  efeitos_marginais$ID[i] <- i
  efeitos_marginais$Efeito[i] <- (y_temp - y_base)
}

efeito_medio_2009_2012 <- format(mean(efeitos_marginais$Efeito), digits = 4)

efeito_medio_pos_intervencao_2009_2012 <- format(mean(subset(efeitos_marginais, ID > 85)$Efeito, digits = 4))

efeito_medio_2009_2012 <- as.numeric(efeito_medio_2009_2012)

efeito_medio_pos_intervencao_2009_2012 <- as.numeric(efeito_medio_pos_intervencao_2009_2012)

efeitos_marginais$Momento <- ifelse(efeitos_marginais$ID > 85, 
                                    "Pós-Intervenção",
                                    "Pré-Intervenção")

efeitos_marginais_plot <- ggplot(data = efeitos_marginais, aes(x = ID, y = Efeito, colour = Momento)) +
  geom_jitter(size = 1, show.legend = TRUE) +
  labs(x = "Observação", y = "Efeito marginal do tratamento")+
  geom_hline(yintercept= efeito_medio_2009_2012, linetype = "dashed", colour = "blue") +
  geom_hline(yintercept= efeito_medio_pos_intervencao_2009_2012, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = 85, colour = "green") +
  theme_minimal()

ggsave(file = "Figuras/Efeitos_marginais_2009_2012.png")

## PLACEBO 1 - Tratamento sobre prop.casados -  Pesos calculados utilizando entropia

didreg_fe_entropia_placebo <- plm(prop_casados ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                    idade2_media + prop_brancos + nota_media + prop_homens + 
                                    prop_medio_ou_mais + prop_renda_3SM,
                                  model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo,
                                  data = painel_federal_estadual_2009_2012_entropia_placebo)

names(didreg_fe_entropia_placebo$coefficients) <- c("Pós-tratamento",
                                                    "Idade média", "Idade média²", "Prop. brancos", 
                                                    "Nota média", "Prop. homens", "Prop. Ensino Médio+",
                                                    "Prop. Renda até 3SM","Diff-and-diff/Impacto")

## PLACEBO 2 - Tratamento sobre escolaridade da mãe -  Pesos calculados utilizando entropia

didreg_fe_entropia_placebo_2 <- plm(prop_medio_ou_mais ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                      idade2_media + prop_brancos + prop_casados + prop_homens + 
                                      nota_media + prop_renda_3SM,
                                    model = "within", index = c("CO_IES", "Ano"), weights = W2_placebo_2,
                                    data = painel_federal_estadual_2009_2012_entropia_placebo_2)

names(didreg_fe_entropia_placebo_2$coefficients) <- c("Pós-tratamento",
                                                      "Idade média", "Idade média²", "Prop. brancos", 
                                                      "Prop. casados", "Prop. homens", "Nota média",
                                                      "Prop. renda até 3SM","Diff-and-diff/Impacto")

#### Exportando os resultados ####

# stargazer(didreg, didreg_fe, didreg_fe_logit, didreg_fe_entropia,
#           type = "latex",
#           title = "Efeito médio de tratamento sobre os tratados - Federal x Estadual - Ciclo 2009-2012",
#           decimal.mark = ",", digits = 3,
#           out = "LaTeX/Federal x Estadual/Resultados_federal_estadual_2009_2012.tex", out.header = TRUE,
#           column.labels = c("Diff-diff",
#                             "Diff-diff + FE",
#                             "Diff-diff + FE + PSM (logit)",
#                             "Diff-diff + FE + PSM (entropia)"),
#           model.numbers = FALSE,
#           flip = TRUE, align = TRUE,
#           float = TRUE, float.env = "sidewaystable",
#           initial.zero = TRUE, intercept.bottom = TRUE,
#           multicolumn = TRUE,model.names = FALSE)

# stargazer(didreg_fe_entropia_placebo, didreg_fe_entropia_placebo_2,
# type = "latex",
# title = "Placebo - Tendências paralelas - Federal x Estadual - Ciclo 2009-2012",
# decimal.mark = ",", digits = 3,
# out = "LaTeX/Federal x Estadual/placebo_2009_2012.tex", out.header = TRUE,
# model.numbers = TRUE,
# align = TRUE,
# float = TRUE, float.env = "table",
# initial.zero = TRUE, intercept.bottom = TRUE,
# multicolumn = TRUE, model.names = FALSE,
# font.size = "small")


#### **Ciclo 2010-2013** ####

painel_federal_estadual_2010_2013 <- subset(painel_federal_estadual,
                                            Ano %in% c(2010, 2013))
painel_federal_estadual_2010_2013$log_nota <- log(painel_federal_estadual_2010_2013$nota_media)

federal_estadual_2010 <- subset(painel_federal_estadual_2010_2013, Ano == 2010)

#### Hipótese do suporte comum ####

ps2 <- glm(data = painel_federal_estadual_2010_2013,
           Tratamento ~ idade_media + idade2_media + prop_brancos +
             prop_casados + prop_homens + prop_medio_ou_mais + 
             prop_renda_3SM, family = binomial(link = "logit"))

painel_federal_estadual_2010_2013$pscore <- predict(ps2, type = "response")

grafico_suporte_comum_2010_2013 <- ggplot(painel_federal_estadual_2010_2013,
                                          aes(x = pscore, fill = Tratamento))+
  geom_density(alpha = 0.4) +
  labs(x = "Propensity score",
       y = "Densidade", 
       fill = "Grupo")+
  theme_minimal()

grafico_suporte_comum_2010_2013 <- grafico_suporte_comum_2010_2013 + 
  scale_fill_discrete(labels=c("Controle",
                               "Tratamento"))

ggsave(file = "Figuras/suporte_comum_2010_2013.png")

## Suporte comum por quantil 

painel_federal_estadual_2010_2013$quantil = with(painel_federal_estadual_2010_2013,
                                                 cut(pscore,
                                                     quantile(pscore,
                                                              prob = seq(0, 0.984, length = 5),
                                                              type = 5)))

media_por_quantil_2010_2013 <- subset(painel_federal_estadual_2010_2013, !is.na(quantil)) %>%
  group_by(quantil, Tratamento) %>%
  summarise_at(vars(pscore), funs(mean(., na.rm=TRUE)))


grafico_suporte_comum_2010_2013_quantil <- ggplot(subset(painel_federal_estadual_2010_2013, !is.na(quantil)),
                                                  aes(x = pscore, fill = Tratamento,
                                                      color = quantil))+
  geom_density(alpha = 0.4) +
  labs(x = "Propensity score",
       y = "Densidade", 
       fill = "Grupo",
       colour = "Quantil")+
  theme_minimal()

grafico_suporte_comum_2010_2013_quantil <- grafico_suporte_comum_2010_2013_quantil + 
  scale_fill_discrete(labels=c("Controle",
                               "Tratamento"))
grafico_suporte_comum_2010_2013_quantil + facet_grid(quantil ~ .)

ggsave(file = "Figuras/suporte_comum_2010_2013_quantis.png")


#### Modelo diff-diff simples ####

didreg_2 = lm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                idade2_media + prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = painel_federal_estadual_2010_2013)

names(didreg_2$coefficients) <- c("Constante", "Tratamento", "Pós-tratamento",
                                  "Idade média", "Idade média²", "Prop. brancos", 
                                  "Prop. casados", "Prop. homens", "Prop. Ensino Médio+",
                                  "Prop. Renda até 3SM","Diff-and-diff/Impacto")

#### Modelo diff-and-diff com efeitos fixos ####

didreg_fe_2 <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                     idade2_media + prop_brancos + prop_casados + prop_homens + 
                     prop_medio_ou_mais + prop_renda_3SM,
                   model = "within", index = c("CO_IES", "Ano"),
                   data = painel_federal_estadual_2010_2013)

names(didreg_fe_2$coefficients) <- c("Pós-tratamento",
                                     "Idade média", "Idade média²", "Prop. brancos", 
                                     "Prop. casados", "Prop. homens", "Prop. Ensino Médio+",
                                     "Prop. Renda até 3SM","Diff-and-diff/Impacto")

#### Modelo diff-and-diff com efeitos fixos e PSM ####

#---- 1. Criação dos pesos utilizando o modelo logit multinomial #----

W3 = weightit(Tratamento ~ idade_media + idade2_media  + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2010,
              method = "ps",
              estimand = "ATT")

#==== 1.1) Checagem do balanceamento #====

b3 = bal.tab(W3, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b3)

love.plot(W3, stats = c("m", "v"), drop.distance = TRUE,
          thresholds = c(m=0.1, v = 0.05),
          wrap = 10,
          var.order = "unadjusted",
          sample.names = c("Não pareado", "Pareado"),
          title = NULL,
          subtitle = "Amostra", line= T,
          var.names = names,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"))

#---- 2. Criação dos pesos utilizando o modelo de entropia #----

W4 = weightit(Tratamento ~ idade_media + idade2_media + 
                prop_brancos + prop_casados + prop_homens + 
                prop_medio_ou_mais + prop_renda_3SM,
              data = federal_estadual_2010,
              method = "ebal",
              estimand = "ATT")

W4_placebo <- weightit(Tratamento ~ idade_media + idade2_media + 
                         prop_brancos + nota_media + prop_homens + 
                         prop_medio_ou_mais + prop_renda_3SM,
                       data = federal_estadual_2010,
                       method = "ebal",
                       estimand = "ATT")

W4_placebo_2 <- weightit(Tratamento ~ idade_media + idade2_media + 
                           prop_brancos + prop_casados + prop_homens + 
                           nota_media + prop_renda_3SM,
                         data = federal_estadual_2010,
                         method = "ebal",
                         estimand = "ATT")



#==== 2.1) Checagem do balanceamento #====

b4 = bal.tab(W4, un = TRUE,
             m.threshold=0.1,
             v.threshold=3)
print(b4)

love.plot(W4, stats = c("m", "v"), drop.distance = TRUE,
          thresholds = c(m=0.1, v = 0.05),
          wrap = 10,
          var.order = "unadjusted",
          sample.names = c("Não pareado", "Pareado"),
          title = NULL,
          subtitle = "Amostra", line= T,
          var.names = names,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"))

b4_placebo = bal.tab(W4_placebo, un = TRUE,
                     m.threshold=0.1,
                     v.threshold=3)
print(b4_placebo)

love.plot(W4_placebo, stats = c("m", "v"), drop.distance = TRUE,
          thresholds = c(m=0.1, v = 0.05),
          wrap = 10,
          var.order = "unadjusted",
          sample.names = c("Não pareado", "Pareado"),
          title = NULL,
          subtitle = "Amostra", line= T,
          var.names = names_placebo,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"))

b4_placebo_2 = bal.tab(W4_placebo_2, un = TRUE,
                       m.threshold=0.1,
                       v.threshold=3)
print(b4_placebo_2)

love.plot(W4_placebo_2, stats = c("m", "v"), drop.distance = TRUE,
          thresholds = c(m=0.1, v = 0.05),
          wrap = 10,
          var.order = "unadjusted",
          sample.names = c("Não pareado", "Pareado"),
          title = NULL,
          subtitle = "Amostra", line= T,
          var.names = names_placebo,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"))

#### Unindo os pesos ao data frame ####

W3_matrix <- data.frame(CO_IES = federal_estadual_2010$CO_IES,
                        W3 = W3$weights)

painel_federal_estadual_2010_2013_logit <- left_join(painel_federal_estadual_2010_2013,
                                                     W3_matrix, by = "CO_IES")

W4_matrix <- data.frame(CO_IES = federal_estadual_2010$CO_IES,
                        W4 = W4$weights)

painel_federal_estadual_2010_2013_entropia <- left_join(painel_federal_estadual_2010_2013,
                                                        W4_matrix, by = "CO_IES")

W4_placebo_matrix <- data.frame(CO_IES = federal_estadual_2010$CO_IES,
                                W4_placebo = W4_placebo$weights)

painel_federal_estadual_2010_2013_entropia_placebo <- left_join(painel_federal_estadual_2010_2013,
                                                                W4_placebo_matrix, by = "CO_IES")

W4_placebo_2_matrix <- data.frame(CO_IES = federal_estadual_2010$CO_IES,
                                  W4_placebo_2 = W4_placebo_2$weights)

painel_federal_estadual_2010_2013_entropia_placebo_2 <- left_join(painel_federal_estadual_2010_2013,
                                                                  W4_placebo_2_matrix, by = "CO_IES")

## Pesos calculados utilizando logit multinomial

didreg_fe_logit_2 <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                           idade2_media + prop_brancos + prop_casados + prop_homens + 
                           prop_medio_ou_mais + prop_renda_3SM,
                         model = "within", index = c("CO_IES", "Ano"), weights = W3,
                         data = painel_federal_estadual_2010_2013_logit)

names(didreg_fe_logit_2$coefficients) <- c("Pós-tratamento",
                                           "Idade média", "Idade média²", "Prop. brancos", 
                                           "Prop. casados", "Prop. homens", "Prop. Ensino Médio+",
                                           "Prop. Renda até 3SM","Diff-and-diff/Impacto")

## Pesos calculados utilizando entropia

didreg_fe_entropia_2 <- plm(log_nota ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                              idade2_media + prop_brancos + prop_casados + prop_homens + 
                              prop_medio_ou_mais + prop_renda_3SM,
                            model = "within", index = c("CO_IES", "Ano"), weights = W4,
                            data = painel_federal_estadual_2010_2013_entropia)

names(didreg_fe_entropia_2$coefficients) <- c("Pós-tratamento",
                                              "Idade média", "Idade média²", "Prop. brancos", 
                                              "Prop. casados", "Prop. homens", "Prop. Ensino Médio+",
                                              "Prop. Renda até 3SM","Diff-and-diff/Impacto")

#### Efeitos marginais ####

coeficientes_2010_2013 <- as.numeric(didreg_fe_entropia_2$coefficients)
efeitos_marginais_2010_2013 <- data.frame(ID = rep(NaN, length(fitted.values(didreg_fe_entropia_2))),
                                          Efeito = rep(NaN, length(fitted.values(didreg_fe_entropia_2))))

for (i in 1:length(fitted.values(didreg_fe_entropia_2))){
  
  y_temp <- with(painel_federal_estadual_2010_2013_entropia,
                 exp(coeficientes_2010_2013[1]*as.numeric(Tempo[i]) + coeficientes_2010_2013[2]*idade_media[i] + 
                       coeficientes_2010_2013[3]*idade2_media[i] + 
                       coeficientes_2010_2013[4]*prop_brancos[i] +
                       coeficientes_2010_2013[5]*prop_casados[i] +
                       coeficientes_2010_2013[6]*prop_homens[i] + 
                       coeficientes_2010_2013[7]*prop_medio_ou_mais[i] + 
                       coeficientes_2010_2013[8]*prop_renda_3SM[i] +
                       coeficientes_2010_2013[9]*1*as.numeric(Tempo[i])))
  y_base <- with(painel_federal_estadual_2010_2013_entropia,
                 exp(  coeficientes_2010_2013[1]*as.numeric(Tempo[i]) +
                         coeficientes_2010_2013[2]*idade_media[i] + 
                         coeficientes_2010_2013[3]*idade2_media[i] + 
                         coeficientes_2010_2013[4]*prop_brancos[i] +
                         coeficientes_2010_2013[5]*prop_casados[i] +
                         coeficientes_2010_2013[6]*prop_homens[i] + 
                         coeficientes_2010_2013[7]*prop_medio_ou_mais[i] + 
                         coeficientes_2010_2013[8]*prop_renda_3SM[i] + 
                         coeficientes_2010_2013[9]*0*as.numeric(Tempo[i])))
  efeitos_marginais_2010_2013$ID[i] <- i
  efeitos_marginais_2010_2013$Efeito[i] <- (y_temp-y_base) ## O coeficiente utilizado aqui determina o EM calculado
}

efeito_medio_2010_2013 <- format(mean(efeitos_marginais_2010_2013$Efeito), digits = 4)

efeito_medio_pos_intervencao_2010_2013 <- format(mean(subset(efeitos_marginais_2010_2013, ID > 85)$Efeito, digits = 4))

efeito_medio_2010_2013 <- as.numeric(efeito_medio_2010_2013)

efeito_medio_pos_intervencao_2010_2013 <- as.numeric(efeito_medio_pos_intervencao_2010_2013)

efeitos_marginais_2010_2013$Momento <- ifelse(efeitos_marginais_2010_2013$ID > 85, 
                                              "Pós-Intervenção",
                                              "Pré-Intervenção")

efeitos_marginais_2010_2013_plot <- ggplot(data = efeitos_marginais_2010_2013, aes(x = ID, y = Efeito, colour = Momento)) +
  geom_jitter(size = 1, show.legend = TRUE) +
  labs(x = "Observação", y = "Efeito marginal do tratamento")+
  geom_hline(yintercept= efeito_medio_2010_2013, linetype = "dashed", colour = "blue") +
  geom_hline(yintercept= efeito_medio_pos_intervencao_2010_2013, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = 85, colour = "green") +
  theme_minimal()

ggsave(file = "Figuras/Efeitos_marginais_2010_2013.png")


## PLACEBO 1 - Tratamento sobre prop.casados -  Pesos calculados utilizando entropia

didreg_fe_entropia_2_placebo <- plm(prop_casados ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                      idade2_media + prop_brancos + nota_media + prop_homens + 
                                      prop_medio_ou_mais + prop_renda_3SM,
                                    model = "within", index = c("CO_IES", "Ano"), weights = W4_placebo,
                                    data = painel_federal_estadual_2010_2013_entropia_placebo)

names(didreg_fe_entropia_2_placebo$coefficients) <- c("Pós-tratamento",
                                                      "Idade média", "Idade média²", "Prop. brancos", 
                                                      "Nota média", "Prop. homens", "Prop. Ensino Médio+",
                                                      "Prop. Renda até 3SM","Diff-and-diff/Impacto")

## PLACEBO 2 - Tratamento sobre escolaridade da mãe -  Pesos calculados utilizando entropia

didreg_fe_entropia_2_placebo_2 <- plm(prop_medio_ou_mais ~ Tratamento + Tempo + Tempo*Tratamento + idade_media +
                                        idade2_media + prop_brancos + prop_casados + prop_homens + 
                                        nota_media + prop_renda_3SM,
                                      model = "within", index = c("CO_IES", "Ano"), weights = W4_placebo_2,
                                      data = painel_federal_estadual_2010_2013_entropia_placebo_2)

names(didreg_fe_entropia_2_placebo_2$coefficients) <- c("Pós-tratamento",
                                                        "Idade média", "Idade média²", "Prop. brancos", 
                                                        "Prop. casados", "Prop. homens", "Nota média",
                                                        "Prop. renda até 3SM","Diff-and-diff/Impacto")

#### Exportando os resultados ####

stargazer(didreg_2, didreg_fe_2, didreg_fe_logit_2, didreg_fe_entropia_2,
          type = "latex",
          title = "Efeito médio de tratamento sobre os tratados - Federal x Estadual - Ciclo 2010-2013",
          decimal.mark = ",", digits = 3,
          out = "LaTeX/Federal x Estadual/Resultados_federal_estadual_2010_2013.tex", out.header = TRUE,
          column.labels = c("Diff-diff",
                            "Diff-diff + FE",
                            "Diff-diff + FE + PSM (logit)",
                            "Diff-diff + FE + PSM (entropia)"),
          model.numbers = FALSE,
          flip = TRUE, align = TRUE,
          float = TRUE, float.env = "sidewaystable",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE,model.names = FALSE)

stargazer(didreg_fe_entropia_2_placebo, didreg_fe_entropia_2_placebo_2,
          type = "latex",
          title = "Placebo - Tendências paralelas - Federal x Estadual - Ciclo 2010-2013",
          decimal.mark = ",", digits = 3,
          out = "LaTeX/Federal x Estadual/placebo_2010_2013.tex", out.header = TRUE,
          model.numbers = TRUE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, intercept.bottom = TRUE,
          multicolumn = TRUE, model.names = FALSE,
          font.size = "small")

#### Lista de IES, nome e participação na greve ####

Aderiram <- read_excel("Dados/Federais_que_aderiram.xlsx")

Aderiram$Nome <- toupper(Aderiram$Nome)

lista_IES <- data.frame(CO_IES = unique(painel_federal_estadual$CO_IES),
                        Nome = unique(painel_federal_estadual$Nome))
lista_IES <- left_join(lista_IES, Aderiram, by = c("Nome" = "Nome"))
lista_IES$Aderiram <- ifelse(is.na(lista_IES$Aderiram), "Não", "Sim")

stargazer(lista_IES[,c(2,3)], type = "latex", 
          title = "Lista de instituições de ensino superior com dummy de aderência à greve",
          decimal.mark = ",", digits = 3,
          out = "LaTeX/Federal x Estadual/lista_ies.tex", out.header = TRUE, rownames = FALSE,
          align = TRUE,
          float = TRUE, float.env = "table",
          initial.zero = TRUE, summary = FALSE)
