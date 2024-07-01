# Carregando bibliotecas -----------------------------------------------------
library(tidyverse)    ### Coleção de pacotes
library(janitor)      ### Manipular variáveis
library(haven)        ### Ler o pacote .sav    
library(labelled)     ### Obter o dicionário das variáveis  
library(survival)     ### Análise de sobrevivência
library(summarytools) ### Função "freq"
library(survminer)    ### Visualização para análise de sobrevivência
library(fastDummies)  ### Criação de dummies
library(randomForestSRC) ### Floresta Aleatória de Sobrevivência
library(corrr)
# Tratamento da base de dados---------------------------------------------------
## Importação da base de dados + clean_names()
link <- ("https://github.com/joaogtrm/ic_amament/raw/main/dados-amamentacao.sav")
df <- read_sav(link)
df <- df %>% 
  janitor::clean_names() %>%
  dplyr::filter(!is.na(tempo_vida_ate_parada_amament_ate180)==TRUE) %>% 
  dplyr::filter(!is.na(censura)==TRUE) %>% 
  dplyr::rename(tempo=tempo_vida_ate_parada_amament_ate180) %>% 
  rename(escolaridade = escolaridade_cat) %>% 
  rename(religiao = religiao_catolica_sn) %>% 
  haven::as_factor() %>% 
  arrange(n_rand)

## Padronização de NA 
df <- df %>% 
  mutate(num_filhos_amamentou_todos = ifelse(df$num_filhos_amamentou_todos == 99997, NA, df$num_filhos_amamentou_todos))

saveRDS(df,"C:/UFES/Projetos/OOBR/ic_amamentacao_2/dados/dados_limpos") # Salvando a base de dados limpa, com todas as variáveis

## Separando nomes das variáveis em relação a sua classe 
###Variáveis numéricas

var_num <- c("idade","horas_trab_dia","salario_familiar","peso_nasc",
  "ganho_peso_por_dia","ig_parto","dias_int_mae_pos_parto","total_t_int",
  "media_amament_prev_meses_todos","censura","tempo")
###Variáveis categóricas
var_fct <- c("escolaridade", "religiao","para_sn","trabalha_sn",
              "mora_com_compan","af_ca_mama","orien_amam_prev","gravid_plan",
              "gravid_desej","tipo_parto","complic_relac_parto",
              "ajuda_para_cuidar","dificuldade_para_amamentar","dor_amamentar",
              "sexo","uti","amament_prev_sn","amamen_exclusiva_sn",
              "retorno_trabalho")
###Variáveis indicadoras ou de identificação
var_id_ind <- c("n_rand","indic_gemelar","indic_mae","indic_info_ganho_peso",
                "indic_t","indic_tempo")

### Criação da base de dados para RSF
df_var_selecionadas <- df %>% select(var_num,var_fct)
df_var_selecionadas <- df_var_selecionadas %>% select(-ganho_peso_por_dia)
df_rsf <- dummy_cols(df_var_selecionadas,remove_first_dummy=T, remove_selected_columns=T) ## Houve um erro na primeira vez que o código do RSF foi executado, então foi necessário criar as dummies de forma manual

saveRDS(df_rsf,"C:/UFES/Projetos/OOBR/ic_amamentacao_2/dados/df_rsf") ## Salvando a base de dados para RSF

### Criação da base de dados para modelagem de Cox
cutpoints <- surv_cutpoint(df, time = "tempo", event = "censura",minprop = 0.05, ### Estimando os pontos de corte ideais
                           variables = var_num)
### Estratificando as variáveis numéricas
df_var_ajustadas <- df[,c('censura',"tempo",var_fct)]
#### Estratificação da variável "idade"
df_var_ajustadas <- df_var_ajustadas %>%
  mutate(idade_menor_igual26 = if_else(df_var_selecionadas$idade <= 26, "MENOR ou igual", "MAIOR")) 
#### Estratificação da variável "horas_trab_dia "
df_var_ajustadas <- df_var_ajustadas %>%
  mutate(horas_trab_maior_7 = if_else(df_var_selecionadas$horas_trab_dia > 7, "MAIOR", "MENOR ou igual")) 
#### Estratificação da variável "salario_familiar"
df_var_ajustadas <- df_var_ajustadas %>% 
  mutate(salario_familiar_menor_1.3 = ifelse(df_var_selecionadas$salario_familiar<1.3 , "MENOR","MAIOR ou igual")) 
#### Estratificação da variável "peso_nasc"
df_var_ajustadas <- df_var_ajustadas %>%
  mutate(peso_nasc_menor2290 = ifelse(df_var_selecionadas$peso_nasc<2290,"MENOR",'MAIOR ou igual'))
#### Estratificação da variável "ig_parto"
df_var_ajustadas <- df_var_ajustadas %>% 
  mutate(ig_parto_menor_36.1 = ifelse(df_var_selecionadas$ig_parto<36.1, "MENOR", "MAIOR ou igual"))
#### Estratificação da variável "dias_int_mae_pos_parto"
df_var_ajustadas <- df_var_ajustadas %>% 
  mutate(dias_int_mae_pos_parto_maior4 = ifelse(df_var_selecionadas$dias_int_mae_pos_parto>4,"MAIOR","MENOR ou igual")) 
#### Estratificação da variável "total_t_int"
df_var_ajustadas <- df_var_ajustadas %>%
  mutate(total_t_int_maior_igual11 = ifelse(df_var_selecionadas$total_t_int>=11,"MAIOR ou igual","MENOR")) 
#### Estratificação da variável "media_amament_prev_mes_todos"
df_var_ajustadas <- df_var_ajustadas %>%
  mutate(media_amament_prev_mes_todos_maior_igual12=ifelse(df_var_selecionadas$media_amament_prev_meses_todos>=12,"MAIOR ou igual","MENOR")) 
#### Estratificação da variável "escolaridade" (o fator "<=8" apresentou uma curva de sobrevivência muito maior que os outros, então também foi escolhido estratificar essa variável)
df_var_ajustadas <- df_var_ajustadas %>%
  mutate(escolaridade = case_when(
    df_var_ajustadas$escolaridade == "<=8" ~ "<=8",
    TRUE ~ ">8"
  ))
### Ajustando classe das variáveis
df <- df_var_ajustadas %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.numeric), as.integer),
         across(where(is.double), as.double))
### salvando a base
saveRDS(df, "C:/UFES/Projetos/OOBR/ic_amamentacao_2/dados/dados_cox")

# Etapa de Análise de sobrevivência-----------------------------------------------------

## Lendo a base de dados
rm(list=ls()) ## Não será preciso continuar com os objetos previamente salvos, apenas a base, que será carregada novamente
dados_ic <- read_rds("C:/UFES/Projetos/OOBR/ic_amamentacao_2/dados/dados_cox")

## Teste de log-rank-------------------------
## LOOP para obter o valor-P de todas as variáveis da base, em relacão ao teste de log-rank. Os resultados serão salvos como dois data frames, um com os resultados de todas as variáveis, e outro com os resultados das variáveis marginalmente significantes
log_rank_p <- as.data.frame(list()) %>%  mutate(variavel=NA,valor_p=NA) #
log_rank_p_sig<- as.data.frame(list()) %>%  mutate(variavel=NA,valor_p=NA)
p_valor_logr_sig <- list()
lrv <- NULL
for (i in (names(dados_ic %>% dplyr::select(-c(tempo,censura) ) ) ) ){
  lrv[[i]]<- survdiff(Surv(dados_ic$tempo,dados_ic$censura)~dados_ic[[i]],rho = 0)
  log_rank_p <- rbind(log_rank_p, data.frame(variavel = i, valor_p =round(lrv[[i]]$pvalue,3),log_rank=round(lrv[[i]]$chisq,3))) 
  if ((lrv[[i]]$pvalue<=.3)==T){
    log_rank_p_sig <- rbind(log_rank_p_sig, data.frame(variavel = i, valor_p =round(lrv[[i]]$pvalue,3),log_rank=round(lrv[[i]]$chisq,3))) 
  }
}
saveRDS(log_rank_p,"C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/testes de log-rank/log_rank de todas as variaveis")
saveRDS(log_rank_p_sig,"C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/testes de log-rank/log_rank das variaveis marg sig")
## Modelagem de Cox-------------------------------------------------------------
rm(list=ls()) ## Não será preciso continuar com os objetos previamente salvos, apenas a base, que será carregada novamente
dados_ic <- read_rds("C:/UFES/Projetos/OOBR/ic_amamentacao_2/dados/dados_cox")
### Ajuste do primeiro modelo de Cox, utilizando todas as variáveis. Após esse primeiro ajuste, será identificada a variável que apresentou o menor p-valor e será ajustado um novo modelo sem essa variável. Esse processo continuará até que se tenha apenas variáveis significantes
modelo_cox1 <- coxph(formula = Surv(dados_ic$tempo,dados_ic$censura)~
                       idade_menor_igual26+
                       religiao+
                       escolaridade+
                       ajuda_para_cuidar+
                       horas_trab_maior_7+
                       retorno_trabalho+
                       salario_familiar_menor_1.3+
                       peso_nasc_menor2290+
                       ig_parto_menor_36.1+
                       complic_relac_parto+
                       dias_int_mae_pos_parto_maior4+
                       total_t_int_maior_igual11+
                       uti+
                       media_amament_prev_mes_todos_maior_igual12+
                       orien_amam_prev+
                       amamen_exclusiva_sn+
                       dificuldade_para_amamentar+
                       dor_amamentar,
                     data = dados_ic,ties = "breslow" )
summary(modelo_cox1)
estimativas_modelo_1 <- as.data.frame(summary(modelo_cox1)[["conf.int"]]) %>% dplyr::select(-"exp(-coef)") %>% mutate(Valor_P=round(as.data.frame(summary(modelo_cox1)[["coefficients"]])[,5],3))%>% arrange(desc(Valor_P))
estimativas_modelo_1

### Modelo 2
modelo_cox2 <- coxph(formula = Surv(dados_ic$tempo,dados_ic$censura)~
                       idade_menor_igual26+
                       religiao+
                       #escolaridade+
                       ajuda_para_cuidar+
                       horas_trab_maior_7+
                       retorno_trabalho+
                       salario_familiar_menor_1.3+
                       peso_nasc_menor2290+
                       ig_parto_menor_36.1+
                       complic_relac_parto+
                       dias_int_mae_pos_parto_maior4+
                       total_t_int_maior_igual11+
                       uti+
                       media_amament_prev_mes_todos_maior_igual12+
                       orien_amam_prev+
                       amamen_exclusiva_sn+
                       dificuldade_para_amamentar+
                       dor_amamentar,
                     data = dados_ic,ties = "breslow" )
summary(modelo_cox2)
estimativas_modelo_2 <- as.data.frame(summary(modelo_cox2)[["conf.int"]]) %>% dplyr::select(-"exp(-coef)") %>% mutate(Valor_P=round(as.data.frame(summary(modelo_cox2)[["coefficients"]])[,5],3))%>% arrange(desc(Valor_P))
estimativas_modelo_2

### Modelo 3
modelo_cox3 <- coxph(formula = Surv(dados_ic$tempo,dados_ic$censura)~
                       #idade_menor_igual26+
                       religiao+
                       #escolaridade+
                       ajuda_para_cuidar+
                       horas_trab_maior_7+
                       retorno_trabalho+
                       salario_familiar_menor_1.3+
                       peso_nasc_menor2290+
                       ig_parto_menor_36.1+
                       complic_relac_parto+
                       dias_int_mae_pos_parto_maior4+
                       total_t_int_maior_igual11+
                       uti+
                       media_amament_prev_mes_todos_maior_igual12+
                       orien_amam_prev+
                       amamen_exclusiva_sn+
                       dificuldade_para_amamentar+
                       dor_amamentar,
                     data = dados_ic,ties = "breslow" )
summary(modelo_cox3)
estimativas_modelo_3 <- as.data.frame(summary(modelo_cox3)[["conf.int"]]) %>% dplyr::select(-"exp(-coef)") %>% mutate(Valor_P=round(as.data.frame(summary(modelo_cox3)[["coefficients"]])[,5],3))%>% arrange(desc(Valor_P))
estimativas_modelo_3

### Modelo 4
modelo_cox4 <- coxph(formula = Surv(dados_ic$tempo,dados_ic$censura)~
                       #idade_menor_igual26+
                       #religiao+
                       #escolaridade+
                       ajuda_para_cuidar+
                       horas_trab_maior_7+
                       retorno_trabalho+
                       salario_familiar_menor_1.3+
                       peso_nasc_menor2290+
                       ig_parto_menor_36.1+
                       complic_relac_parto+
                       dias_int_mae_pos_parto_maior4+
                       total_t_int_maior_igual11+
                       uti+
                       media_amament_prev_mes_todos_maior_igual12+
                       orien_amam_prev+
                       amamen_exclusiva_sn+
                       dificuldade_para_amamentar+
                       dor_amamentar,
                     data = dados_ic,ties = "breslow" )
summary(modelo_cox4)
estimativas_modelo_4 <- as.data.frame(summary(modelo_cox4)[["conf.int"]]) %>% dplyr::select(-"exp(-coef)") %>% mutate(Valor_P=round(as.data.frame(summary(modelo_cox4)[["coefficients"]])[,5],3))%>% arrange(desc(Valor_P))
estimativas_modelo_4

### Modelo 5
modelo_cox5 <- coxph(formula = Surv(dados_ic$tempo,dados_ic$censura)~
                       #idade_menor_igual26+
                       #religiao+
                       #escolaridade+
                       ajuda_para_cuidar+
                       horas_trab_maior_7+
                       retorno_trabalho+
                       salario_familiar_menor_1.3+
                       peso_nasc_menor2290+
                       ig_parto_menor_36.1+
                       complic_relac_parto+
                       dias_int_mae_pos_parto_maior4+
                       total_t_int_maior_igual11+
                       #uti+
                       media_amament_prev_mes_todos_maior_igual12+
                       orien_amam_prev+
                       amamen_exclusiva_sn+
                       dificuldade_para_amamentar+
                       dor_amamentar,
                     data = dados_ic,ties = "breslow" )
summary(modelo_cox5)
estimativas_modelo_5 <- as.data.frame(summary(modelo_cox5)[["conf.int"]]) %>% dplyr::select(-"exp(-coef)") %>% mutate(Valor_P=round(as.data.frame(summary(modelo_cox5)[["coefficients"]])[,5],3))%>% arrange(desc(Valor_P))
estimativas_modelo_5

### Modelo 6
modelo_cox6 <- coxph(formula = Surv(dados_ic$tempo,dados_ic$censura)~
                       #idade_menor_igual26+
                       #religiao+
                       #escolaridade+
                       ajuda_para_cuidar+
                       horas_trab_maior_7+
                       retorno_trabalho+
                       salario_familiar_menor_1.3+
                       peso_nasc_menor2290+
                       #ig_parto_menor_36.1+
                       complic_relac_parto+
                       dias_int_mae_pos_parto_maior4+
                       total_t_int_maior_igual11+
                       #uti+
                       media_amament_prev_mes_todos_maior_igual12+
                       orien_amam_prev+
                       amamen_exclusiva_sn+
                       dificuldade_para_amamentar+
                       dor_amamentar,
                     data = dados_ic,ties = "breslow" )
summary(modelo_cox6)
estimativas_modelo_6 <- as.data.frame(summary(modelo_cox6)[["conf.int"]]) %>% dplyr::select(-"exp(-coef)") %>% mutate(Valor_P=round(as.data.frame(summary(modelo_cox6)[["coefficients"]])[,5],3))%>% arrange(desc(Valor_P))
estimativas_modelo_6

### Modelo 7
modelo_cox7 <- coxph(formula = Surv(dados_ic$tempo,dados_ic$censura)~
                       #idade_menor_igual26+
                       #religiao+
                       #escolaridade+
                       ajuda_para_cuidar+
                       horas_trab_maior_7+
                       retorno_trabalho+
                       salario_familiar_menor_1.3+
                       peso_nasc_menor2290+
                       #ig_parto_menor_36.1+
                       complic_relac_parto+
                       dias_int_mae_pos_parto_maior4+
                       #total_t_int_maior_igual11+
                       #uti+
                       media_amament_prev_mes_todos_maior_igual12+
                       orien_amam_prev+
                       amamen_exclusiva_sn+
                       dificuldade_para_amamentar+
                       dor_amamentar,
                     data = dados_ic,ties = "breslow" )
summary(modelo_cox7)
estimativas_modelo_7 <- as.data.frame(summary(modelo_cox7)[["conf.int"]]) %>% dplyr::select(-"exp(-coef)") %>% mutate(Valor_P=round(as.data.frame(summary(modelo_cox7)[["coefficients"]])[,5],3))%>% arrange(desc(Valor_P))
estimativas_modelo_7

### Modelo 8
modelo_cox8 <- coxph(formula = Surv(dados_ic$tempo,dados_ic$censura)~
                       #idade_menor_igual26+
                       #religiao+
                       #escolaridade+
                       ajuda_para_cuidar+
                       horas_trab_maior_7+
                       retorno_trabalho+
                       salario_familiar_menor_1.3+
                       peso_nasc_menor2290+
                       #ig_parto_menor_36.1+
                       #complic_relac_parto+
                       dias_int_mae_pos_parto_maior4+
                       #total_t_int_maior_igual11+
                       #uti+
                       media_amament_prev_mes_todos_maior_igual12+
                       orien_amam_prev+
                       amamen_exclusiva_sn+
                       dificuldade_para_amamentar+
                       dor_amamentar,
                     data = dados_ic,ties = "breslow" )
summary(modelo_cox8)
estimativas_modelo_8 <- as.data.frame(summary(modelo_cox8)[["conf.int"]]) %>% dplyr::select(-"exp(-coef)") %>% mutate(Valor_P=round(as.data.frame(summary(modelo_cox8)[["coefficients"]])[,5],3))%>% arrange(desc(Valor_P))
estimativas_modelo_8

### Modelo completo-----------------------------------------------------------
modelo_cox9 <- coxph(formula = Surv(dados_ic$tempo,dados_ic$censura)~
                       #idade_menor_igual26+
                       #religiao+
                       #escolaridade+
                       ajuda_para_cuidar+
                       #horas_trab_maior_7+
                       retorno_trabalho+
                       salario_familiar_menor_1.3+
                       peso_nasc_menor2290+
                       #ig_parto_menor_36.1+
                       #complic_relac_parto+
                       dias_int_mae_pos_parto_maior4+
                       #total_t_int_maior_igual11+
                       #uti+
                       media_amament_prev_mes_todos_maior_igual12+
                       orien_amam_prev+
                       amamen_exclusiva_sn+
                       dificuldade_para_amamentar+
                       dor_amamentar,
                     data = dados_ic,ties = "breslow" )
### Data frame com as estimativas do modelo completo de cox
estimativas_modelo_9 <- as.data.frame(summary(modelo_cox9)[["conf.int"]]) %>% 
  dplyr::select(-"exp(-coef)") %>% 
  mutate(Valor_P=round(as.data.frame(summary(modelo_cox9)[["coefficients"]])[,5],3))#%>% arrange(desc(Valor_P))
estimativas_modelo_9$`lower .95` <- round(estimativas_modelo_9$`lower .95`,3)
#estimativas_modelo_9$`lower .95` <- ifelse(estimativas_modelo_9$`lower .95` < 1, 
#                                           round(1 / estimativas_modelo_9$`lower .95`, 3), 
#                                           estimativas_modelo_9$`lower .95`)
estimativas_modelo_9$`upper .95` <- round(estimativas_modelo_9$`upper .95`,3)
#estimativas_modelo_9$`upper .95` <- ifelse(estimativas_modelo_9$`upper .95` < 1, 
#                                           round(1 / estimativas_modelo_9$`upper .95`, 3), 
#                                           estimativas_modelo_9$`upper .95`)
estimativas_modelo_9$`exp(coef)` <- round(estimativas_modelo_9$`exp(coef)`,3)
#estimativas_modelo_9$`exp(coef)` <- ifelse(estimativas_modelo_9$`exp(coef)` < 1, 
#                                           round(1 / estimativas_modelo_9$`exp(coef)`, 3), 
#                                           estimativas_modelo_9$`exp(coef)`)
estimativas_modelo_9 <-  estimativas_modelo_9 %>% mutate(range_bounds = abs((estimativas_modelo_9$`upper .95`-estimativas_modelo_9$`lower .95`)))
### Salvando o modelo final e suas estimativas
saveRDS(modelo_cox9,"C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/modelo de cox final")
saveRDS(estimativas_modelo_9,"C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/estimativas do modelo completo")

#### Teste de proporcionalidade do modelo de cox final----------------------------
#### Teste estatístico 
teste_phz9<- cox.zph(modelo_cox9,transform="km")
teste_phz9 # Nenhuma variável rejeitou a hipótese nula, ou seja, não há evidência que as taxas de riscos das variáveis não sejam proporcionais em relação ao tempo
saveRDS(teste_phz9,"C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/teste de proporcionalidade do modelo de cox final")

#### Plotando e salvando os gráficos dos resíduos para as variáveis do modelo final 
label_font_size <- 1.3
axis_font_size <- 1.3
cex.lab_valor <- 1.3
cex.axis_valor <- 1.5

jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/ajuda_para_cuidar_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[1,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)

dev.off()
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/retorno_trabalho_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[2,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)

dev.off()
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/salario_familiar_menor_1.3_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[3,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)

dev.off()
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/peso_nasc_menor2290_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[4,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)

dev.off()
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/dias_int_mae_pos_parto_maior4_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[5,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)

dev.off()
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/media_amament_prev_mes_todos_maior_igual12_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[6,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)

dev.off()
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/orien_amam_prev_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[7,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)

dev.off()
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/amamen_exclusiva_sn_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[8,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)

dev.off()
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/dificuldade_para_amamentar_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[9,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)

dev.off()
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/Residuo/dor_amamentar_cox.jpeg",width = 1000,height = 500,units = "px")
plot(teste_phz9[10,], cex.lab = label_font_size, cex.axis = axis_font_size)
par(cex.axis = cex.axis_valor, cex.lab = cex.lab_valor)
dev.off()

## Modelo de RSF-------------------------------------------------------
rm(list=ls()) ## Não será preciso continuar com os objetos previamente salvos, apenas a base, que será carregada novamente
###  Modelo fit1: modelo usando base de dados completa, com variáveis estratificadas-------------------------------------------------------
#### lendo a base de dados 
df <- readRDS("C:/UFES/Projetos/OOBR/ic_amamentacao_2/dados/dados_cox")
df <- dummy_cols(df,remove_first_dummy=T, remove_selected_columns=T)


#### tunando os hiperparâmetros de fit1
set.seed(22)
fit_tune1 <- tune(Surv(tempo, censura) ~ ., data = df, doBest = TRUE,ntreeTry=100)
fit_tune1$optimal
print(fit_tune1)
fit_tune1$optimal
fit_tune1$rf
#### ajustando
set.seed(22)
fit1 <- rfsrc(Surv(tempo, censura) ~ ., data = df, mtry = fit_tune1$optimal[2], nodesize = fit_tune1$optimal[1],block.size = 1, importance = TRUE)
print(fit1)
saveRDS(fit1,"C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/RSF/fit1")
#### (OOB) CRPS (menor, melhor): 0.0822992; (OOB) Performance error (1 - C index; menor, melhor): 0.16500272

#### plot fit1 
plot(fit1)
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/RSF/plots/plot_fit1.jpeg", width = 1000, height = 500, units = "px")
plot(fit1)
dev.off()


### Modelo fit2: modelo usando base de dados completa, sem variáveis estratificadas---------------
#### lendo a base de dados 
df <- readRDS("C:/UFES/Projetos/OOBR/ic_amamentacao_2/dados/df_rsf")
#### tunando os hiperparâmetros de fit2

set.seed(22)
fit_tune2 <- tune(Surv(tempo, censura) ~ ., data = df, doBest = TRUE,ntreeTry=100)
fit_tune2$optimal
print(fit_tune2)
fit_tune2$optimal
fit_tune2$rf
#### ajustando fit2
set.seed(22)
fit2 <- rfsrc(Surv(tempo, censura) ~ ., data = df, mtry = fit_tune2$optimal[2], nodesize = fit_tune2$optimal[1],block.size = 1, importance = TRUE)
print(fit2)
saveRDS(fit2,"C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/RSF/fit2")
#### (OOB) CRPS (menor, melhor): 0.07496245; (OOB) Performance error (1 - C index; menor, melhor): 0.1402024

### plot fit2 
plot(fit2)
jpeg("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/RSF/plots/plot_fit2.jpeg", width = 1000, height = 500, units = "px")
plot(fit2)
dev.off()

# Comparação entre os modelos---------------------------------------------------
## carregando modelos
mod_cox <- readRDS("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/modelo de cox/modelo de cox final")
mod_rsf <- readRDS("C:/UFES/Projetos/OOBR/ic_amamentacao_2/Resultados/RSF/fit2")
## resultados do modelo de floresta
1-mod_rsf$err.rate[length(mod_rsf$err.rate)]
### C-index: 0.8597976
se_c_index_rsf <- sd(mod_rsf$err.rate)
se_c_index_rsf
### sd(C-index): 0.01746519

## resultados do modelo de Cox
mod_cox$concordance[6] 
### C-index: 0.7628757
mod_cox$concordance[7] 
### sd(C-index): 0.01573079 
