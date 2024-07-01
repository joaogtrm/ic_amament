rm(list=ls())
# bibliotescas 
library(survival)
library(tidyverse)
library(survminer)
par(mfrow = c(2, 2))
# lendo a base de dados
setwd("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/graficos_km")
dados <- read_rds("C:/UFES/Projetos/OOBR/ic_amamentacao_2/dados/dados_cox")

# Gerando e salvando gráficos de Kaplan-Meier
graficos_km_salvos <- list()
lista_com_variaveis <- list()
variavel_nome<-NA

###########################################

variavel_nome <- "para_sn"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], data = dados,
                                           pval = FALSE, 
                                           conf.int=FALSE,
                                           pval.method=TRUE, 
                                           conf.int.style = "step",
                                           xlim=c(0,180),
                                           break.x.by = 30,
                                           break.y.by = 0.1,
                                           ylab = "Sobrevida", 
                                           xlab = "tempo em dias",
                                          # legend.title = "Paridade:", 
                                           legend.labs = c("Não","Sim"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                      font.main = c(25),
                                      font.x = c(25),
                                      font.y = c(25),
                                      font.caption = c(25), 
                                      font.legend = c(25), 
                                      font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("para_sn.jpeg",width = 20, height = 20,units = "cm")

###########################################

variavel_nome <- "trabalha_sn"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                           data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Trabalha:", 
                                                 legend.labs = c("Não","Sim"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("trabalha_sn.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "mora_com_compan"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Mora com o companheiro:", 
                                                 legend.labs = c("Não","Sim"))

graficos_km_salvos[["mora_com_compan"]] <- ggpar(graficos_km_salvos[["mora_com_compan"]], 
                                              font.main = c(25),
                                              font.x = c(25),
                                              font.y = c(25),
                                              font.caption = c(25), 
                                              font.legend = c(25), 
                                              font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("mora_com_compan.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "af_ca_mama"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Tem, ou teve, câncer de mama:", 
                                                 legend.labs = c("Não","Sim"))

graficos_km_salvos[["af_ca_mama"]] <- ggpar(graficos_km_salvos[["af_ca_mama"]], 
                                         font.main = c(25),
                                         font.x = c(25),
                                         font.y = c(25),
                                         font.caption = c(25), 
                                         font.legend = c(25), 
                                         font.tickslab = c(25))

ggsave("af_ca_mama.jpeg",width = 20,height = 20,units = "cm")
       
###########################################

variavel_nome <- "gravid_plan"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Gravidez planejada:", 
                                                 legend.labs = c("Não","Sim"))

graficos_km_salvos[["gravid_plan"]] <- ggpar(graficos_km_salvos[["gravid_plan"]], 
                                          font.main = c(25),
                                          font.x = c(25),
                                          font.y = c(25),
                                          font.caption = c(25), 
                                          font.legend = c(25), 
                                          font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("gravid_plan.jpeg",width = 20,height = 20,units = "cm")


###########################################

variavel_nome <- "gravid_desej"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 legend.title = "Gravidez desejada:", 
                                                 legend.labs = c("Não","Sim"))

graficos_km_salvos[["gravid_desej"]] <- ggpar(graficos_km_salvos[["gravid_desej"]], 
                                           font.main = c(25),
                                           font.x = c(25),
                                           font.y = c(25),
                                           font.caption = c(25), 
                                           font.legend = c(25), 
                                           font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]

ggsave("gravid_desej.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "sexo"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 
                                                 #legend.title = "Sexo do recém-nascido:", 
                                                 legend.labs = c("Feminino","Masculino"))

graficos_km_salvos[["sexo"]] <- ggpar(graficos_km_salvos[["sexo"]], 
                                   font.main = c(25),
                                   font.x = c(25),
                                   font.y = c(25),
                                   font.caption = c(25), 
                                   font.legend = c(25), 
                                   font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("sexo.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "tipo_parto"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Tipo de parto:", 
                                                 legend.labs = c("Cesarea","Normal"))
graficos_km_salvos[["tipo_parto"]] <- ggpar(graficos_km_salvos[["tipo_parto"]], 
                                  font.main = c(25),
                                  font.x = c(25),
                                  font.y = c(25),
                                  font.caption = c(25), 
                                  font.legend = c(25), 
                                  font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("tipo_parto.jpeg",width = 20,height = 20,units = "cm")
###########################################

variavel_nome <- "amament_prev_sn"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Amamentou previamente:", 
                                                 legend.labs = c("Não","Sim"))
graficos_km_salvos[["amament_prev_sn"]] <- ggpar(graficos_km_salvos[["amament_prev_sn"]], 
                                  font.main = c(25),
                                  font.x = c(25),
                                  font.y = c(25),
                                  font.caption = c(25), 
                                  font.legend = c(25), 
                                  font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("amament_prev_sn.jpeg",width = 20,height = 20,units = "cm")
###########################################

variavel_nome <- "religiao"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Nenhuma","Católica","Outra"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                  font.main = c(25),
                                  font.x = c(25),
                                  font.y = c(25),
                                  font.caption = c(25), 
                                  font.legend = c(25), 
                                  font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("religiao.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "escolaridade"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("<=8",">8"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                  font.main = c(25),
                                  font.x = c(25),
                                  font.y = c(25),
                                  font.caption = c(25), 
                                  font.legend = c(25), 
                                  font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("escolaridade.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "orien_amam_prev"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Não","Sim"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("orien_amam_prev.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "complic_relac_parto"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida", 
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Não","Sim"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("complic_relac_parto.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "ajuda_para_cuidar"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Não","Sim"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("ajuda_para_cuidar.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "dificuldade_para_amamentar"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Não","Sim"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("dificuldade_para_amamentar.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "dor_amamentar"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Não","Sim"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("dor_amamentar.jpeg",width = 20,height = 20,units = "cm")

###########################################
variavel_nome <- "uti"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Não","Sim"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("uti.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "amamen_exclusiva_sn"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Nunca exclusiva","Exclusiva em algum momento"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("amamen_exclusiva_sn.jpeg",width = 25,height = 20,units = "cm")

###########################################

variavel_nome <- "idade_menor_igual26"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Maior","Menor"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("idade_menor_igual26.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "salario_familiar_menor_1.3"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Maior ou igual","Menor"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("salario_familiar_menor_1.3.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "horas_trab_maior_7"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Maior","Menor ou igual"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("horas_trab_maior_7.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "peso_nasc_menor2290"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Maior ou igual","Menor"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("peso_nasc_menor2290.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "ig_parto_menor_36.1"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Maior ou igual","Menor"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("ig_parto_menor_36.1.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "dias_int_mae_pos_parto_maior4"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Maior","Menor ou igual"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("dias_int_mae_pos_parto_maior4.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "total_t_int_maior_igual11"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Maior ou igual","Menor"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("total_t_int_maior_igual11.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "media_amament_prev_mes_todos_maior_igual12"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Maior ou igual","Menor"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("media_amament_prev_mes_todos_maior_igual12.jpeg",width = 20,height = 20,units = "cm")

###########################################

variavel_nome <- "retorno_trabalho"
lista_com_variaveis[[variavel_nome]] <- survfit(Surv(dados$tempo,dados$censura)~dados[[variavel_nome]])
graficos_km_salvos[[variavel_nome]] <- ggsurvplot(lista_com_variaveis[[variavel_nome]], 
                                                 data = dados,
                                                 pval = FALSE, 
                                                 conf.int=FALSE,
                                                 pval.method=TRUE, 
                                                 conf.int.style = "step",
                                                 xlim=c(0,180),
                                                 break.x.by = 30,
                                                 break.y.by = 0.1,
                                                 ylab = "Sobrevida",
                                                 xlab = "tempo em dias",
                                                 #legend.title = "Teve aconselhamento sobre amamentação durante a pesquisa:", 
                                                 legend.labs = c("Não","Sim"))
graficos_km_salvos[[variavel_nome]] <- ggpar(graficos_km_salvos[[variavel_nome]], 
                                            font.main = c(25),
                                            font.x = c(25),
                                            font.y = c(25),
                                            font.caption = c(25), 
                                            font.legend = c(25), 
                                            font.tickslab = c(25))

graficos_km_salvos[[variavel_nome]]
ggsave("retorno_trabalho.jpeg",width = 20,height = 20,units = "cm")
