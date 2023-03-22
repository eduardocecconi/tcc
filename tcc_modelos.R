library(tidyverse)
library(readxl)
library(writexl)
library(stats)
library(reshape2)
library(scales)
library(PerformanceAnalytics)
library(jtools)
library(ggstance)
library(nortest)
library(car)
library(bestNormalize)
library(olsrr)
library(lmtest)
library(overdisp)
library(MASS)
library(pscl)
library(Hmisc)
library(caret)
library(pROC)
library(ROCR)
library(kableExtra)
library(mpath)
library(nonnest2)
library(nnet)


#SET.SEED PARA GARANTIR REPRODUÇÃO DOS RESULTADOS
set.seed(23)

#FUNÇÃO PARA EVITAR NOTAÇÃO CIENTÍFICA
options(scipen=999)

###CARREGAR ARQUIVOS BASE
  #COM CLUSTERS DE COMPRADOR E VENDEDOR
jogadores_clusters <- read_excel("jogadores_clusters.xlsx") 

  #COM DUMMIES PARA VARIÁVEIS CATEGÓRICAS
jogadores_dummies <- read_excel("jogadores_dummies.xlsx") 

#########################   ESTIMATIVA DE VALOR   ##############################

###DATASET CONSOLIDADO PARA ESTIMATIVA DE VALOR
  #FILTRO: A PARTIR DE 2 JOGOS REGISTRADOS
  #RESULTADO: 
    #48 VARIÁVEIS (1 DEPENDENTE, 47 PREDITORAS - SENDO 18 DUMMIES)
    #820 OBSERVAÇÕES (157 COM VALOR, 663 COM ZEROS - 80.85%)
valor_base <- jogadores_dummies %>% 
  filter(Jogos > 1) %>% 
  rename(Serie_B = `Liga_Série B`,
         Serie_C = `Liga_Série C`) %>% 
  dplyr::select(1:5, 8:13, 15:21, 23:27, 30:41, 47:59)

#EXPORTAÇÃO PARA ARQUIVO XLSX
write_xlsx(valor_base, "valor_base.xlsx")

#ESTUDO DAS CORRELAÇÕES
valor_correl <- rcorr(as.matrix(valor_base), type="pearson")
valor_matriz_correl <- valor_correl$r

################################################################################

###DETECÇÃO DE SUPERDISPERSÃO

#DIAGNÓSTICO PRELIMINAR - MÉDIA vs. VARIÂNCIA
  #RESULTADO: ALTA DIFERENÇA (TENDÊNCIA DE SUPERDISPERSÃO)
    #MÉDIA: 1087428
    #VARIÂNCIA: 19716728298846
valor_base %>%
  summarise(Média = mean(Valor),
            Variância = var(Valor)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F,
                font_size = 30)

#TESTE DE CAMERON-TRIVEDI
  #Resultado: p-valor < 0.05 (0.02008)
  #HÁ superdispersão (alongamento da cauda)
overdisp(valor_base,
         dependent.position = 1,
         predictor.position = 2:48)

################################################################################

###MODELO BINOMIAL NEGATIVO PARA DADOS DE CONTAGEM
  #TODAS AS VARIÁVEIS COM O MESMO P-VALOR (DISPENSA STEPWISE)
  #VALOR DE SE.THETA = NaN
regressao_binomial_neg <- glm.nb(formula = Valor ~ . ,
                                 data = valor_base)

#VISUALIZAÇÃO DO RESULTADO
  #VALOR DE THETA: 671432548.7 (PARA UTILIZAR EM NOVO MODELO)
summary(regressao_binomial_neg) 

#DIAGNÓSTICO DE MULTICOLINEARIDADE
  #SEIS VARIÁVEIS COM VIF > 10
    # Passes_Progressivos_media, Passes_Terco_Final_media, Passes_Area_media
    # Passes_media, Dribles_media, Toques_Area_media
vif(regressao_binomial_neg)

#NOVO MODELO, EM FUNÇÃO DIFERENTE E COM VALOR DE THETA INFORMADO 
regressao_binomial_neg2 <- glm(formula = Valor ~ . , 
                              data = valor_base, 
                              family = negative.binomial(theta = 671432549))

#VISUALIZAÇÃO DO RESULTADO
  # 15 VARIÁVEIS COM P-VALOR < 0.05
    # Idade, Minutos_media, Gols, Passes_Chave_media, Pre_Assistencia_media,
    # Passes_Terco_Final_media, Cruzamentos_media, Faltas_Cometidas_media,
    # Cluster_Vendedor_1, Cluster_Vendedor_2, Cluster_Vendedor_4,
    # Cluster_Vendedor_5, Cluster_Vendedor_6, Posicao_Lateral_Direito, Serie_B
summary(regressao_binomial_neg2) 

#PROCEDIMENTO STEPWISE NÃO CONSEGUE EXCLUIR AS VARIÁVEIS COM P-VALOR > 0.05
step_regressao_binomial_neg2 <- step(regressao_binomial_neg2, k = 3.841459)

#SELEÇÃO MANUAL DAS 15 VARIÁVEIS COM P-VALOR < 0.05
valor_base_bineg <- valor_base %>% 
  dplyr::select(Valor, Idade, Minutos_media, Gols, Passes_Chave_media, 
                Pre_Assistencia_media, Passes_Terco_Final_media, 
                Cruzamentos_media, Faltas_Cometidas_media,
                Cluster_Vendedor_1, Cluster_Vendedor_2, Cluster_Vendedor_4,
                Cluster_Vendedor_5, Cluster_Vendedor_6, 
                Posicao_Lateral_Direito, Serie_B)

#NOVO MODELO COM AS 15 VARIÁVEIS SELECIONADAS E NOVO VALOR DE THETA
regressao_binomial_neg3 <- glm.nb(formula = Valor ~ . ,
                                 data = valor_base_bineg,
                                 init.theta = 1423413)

#VISUALIZAÇÃO DO RESULTADO
  #TODAS AS VARIÁVEIS COM O MESMO P-VALOR (DISPENSA STEPWISE)
summary(regressao_binomial_neg3) 

#DIAGNÓSTICO DE MULTICOLINEARIDADE
  #SOMENTE VALORES BAIXOS DE VIF
vif(regressao_binomial_neg3)

#EXTRAÇÃO DO VALOR DE LOGLIKELIHOOD
  #LOGLIK = -418487238
logLik(regressao_binomial_neg3)

#EXTRAÇÃO DO VALOR DE BIC
  #BIC = 836974590
cat(AIC(regressao_binomial_neg3, k=log(dim(valor_base_bineg)[1])))

#EXTRAÇÃO DO VALOR DE AIC
  #AIC = 836974510
cat(AIC(regressao_binomial_neg3))

#DATAFRAME COM OS 3 INDICADORES
indicadores_binomial_neg <- data.frame(
  Modelo = "BinomialNeg",
  LogLik = -418487238,
  BIC =  836974590,
  AIC = 836974510, 
  row.names = NULL)

#VISUALIZAÇÃO
indicadores_binomial_neg %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)

################################################################################

###MODELO BINOMIAL NEGATIVO ZERO INFLATED
#MODELO SELECIONADO PARA ESTIMATIVA DE VALOR
regressao_binomial_zeroinfl <- zeroinfl(Valor ~ . | . , 
                                        data = valor_base,
                                        dist = "negbin",
                                        link = "logit")

#VISUALIZAÇÃO DO RESULTADO
summary(regressao_binomial_zeroinfl)

#PROCEDIMENTO BACKWARD STEPWISE (5% DE SIGNIFICÂNCIA)
step_regressao_binomial_zeroinfl <- be.zeroinfl(regressao_binomial_zeroinfl, 
                                                data=valor_base, 
                                                dist="negbin", 
                                                alpha=0.05, 
                                                trace=FALSE)

#VISUALIZAÇÃO DO RESULTADO (20 VARIÁVEIS MANTIDAS NO TOTAL)
  #10 VARIÁVEIS NO MODELO COUNT
    # Idade + Jogos + Minutos_media + Gols + Dribles_media + 
    # Passes_Terco_Final_media + Cluster_Vendedor_1 + Cluster_Vendedor_2 + 
    # Cluster_Vendedor_4 + Cluster_Vendedor_6
  #16 VARIÁVEIS NO MODELO ZEROINFL (6 EM COMUM)
    # *Idade + *Jogos + *Minutos_media + Conversao + Toques_Area_media + 
    # Conducoes_media + Faltas_Recebidas_media + Interceptacoes_media + 
    # *Cluster_Vendedor_1 + *Cluster_Vendedor_2 + *Cluster_Vendedor_4 + 
    # Cluster_Vendedor_5 + Posicao_Meia + Posicao_Volante + Serie_B + Serie_C
summary(step_regressao_binomial_zeroinfl)

#EXTRAÇÃO DO VALOR DE LOGLIKELIHOOD - MODELO COM PROCEDIMENTO STEPWISE
#LOGLIK = -2697.25
logLik(step_regressao_binomial_zeroinfl)

#EXTRAÇÃO DO VALOR DE BIC
#BIC = 5589.069
cat(AIC(step_regressao_binomial_zeroinfl, k=log(dim(valor_base)[1])))

#EXTRAÇÃO DO VALOR DE AIC
#AIC = 5452.5
cat(AIC(step_regressao_binomial_zeroinfl))

#DATAFRAME COM OS 3 INDICADORES
indicadores_binomial_zeroinfl <- data.frame(
  Modelo = "BinomialZI",
  LogLik = -2697.25,
  BIC =  5589.069,
  AIC = 5452.5, 
  row.names = NULL)

#VISUALIZAÇÃO
indicadores_binomial_zeroinfl %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)

#SALVAR MODELO PARA CARREGAMENTO POSTERIOR
saveRDS(step_regressao_binomial_zeroinfl, file = "step_regressao_binomial_zeroinfl.rds")

#LIKELIHOOD RATIO TEST BINOMIAL NEGATIVO ZERO-INFLATED E BINOMIAL NEGATIVO
  #Modelo Zero-Inflated obteve melhor LogLik
  #p-valor do chi-quadrado indica diferença significativa entre os modelos
lrtest(step_regressao_binomial_zeroinfl, regressao_binomial_neg3)

#TESTE DE VUONG
  #p-valor do teste de Likelihood < 0.05 (MODELO STEP ZEROINFL TEM MELHOR AJUSTE)
  #p-valor do teste de variância  = 0.05 (MODELOS NÃO SÃO DISTINGUÍVEIS)
vuongtest(step_regressao_binomial_zeroinfl, regressao_binomial_neg3, 
          nested = FALSE,
          adj = "bic")

################################################################################

###MODELO HURDLE
regressao_hurdle_zeroinfl <- hurdle(Valor ~ . , data = valor_base, 
                                    dist = "negbin",
                                    zero.dist = "binomial",
                                    link = "logit")

#VISUALIZAÇÃO DO RESULTADO
summary(regressao_hurdle_zeroinfl)

#PROCEDIMENTO STEPWISE (5% DE SIGNIFICÂNCIA)
step_regressao_hurdle_zeroinfl <- step(regressao_hurdle_zeroinfl, k = 3.841459)

#VISUALIZAÇÃO DO RESULTADO
  #10 VARIÁVEIS MANTIDAS (AS MESMAS NOS MODELOS COUNT E ZEROINFL)
    #Idade + Minutos_media + Toques_Area_media + Faltas_Recebidas_media + 
    #Cluster_Vendedor_1 + Cluster_Vendedor_2 + Cluster_Vendedor_4 + Cluster_Vendedor_5 + 
    #Cluster_Vendedor_6 + Posicao_Meia
summary(step_regressao_hurdle_zeroinfl)

#EXTRAÇÃO DO VALOR DE LOGLIKELIHOOD - MODELO COM PROCEDIMENTO STEP
  #LOGLIK = -2717.578 (LEVEMENTE PIOR QUE O BIN NEG ZEROINFL)
logLik(step_regressao_hurdle_zeroinfl)

#EXTRAÇÃO DO VALOR DE BIC (MUITO MELHOR QUE O POISSON)
  #BIC = 5589.47 (LEVEMENTE PIOR QUE O BIN NEG ZEROINFL)
cat(AIC(step_regressao_hurdle_zeroinfl, k=log(dim(valor_base)[1])))

#EXTRAÇÃO DO VALOR DE AIC (MUITO MELHOR QUE O POISSON)
  #AIC = 5481.156 (LEVEMENTE PIOR QUE O BIN NEG ZEROINFL)
cat(AIC(step_regressao_hurdle_zeroinfl))

#DATAFRAME COM OS 3 INDICADORES
indicadores_hurdle_zeroinfl <- data.frame(
  Modelo = "HurdleZI",
  LogLik = -2717.578,
  BIC =  5589.47,
  AIC = 5481.156, 
  row.names = NULL)

#VISUALIZAÇÃO
indicadores_hurdle_zeroinfl %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)

#LIKELIHOOD RATIO TEST BINOMIAL NEGATIVO ZERO-INFLATED E BINOMIAL NEGATIVO
#Modelo Zero-Inflated obteve melhor LogLik
#p-valor do chi-quadrado indica diferença significativa em relação ao modelo Poisson
lrtest(step_regressao_hurdle_zeroinfl, step_regressao_binomial_zeroinfl)

#####################   DATAFRAMES COMPARATIVOS   ##############################

###DATAFRAME COM A COMPARAÇÃO ENTRE VALOR REAL E VALORES PREDITOS
  #VARIÁVEIS CATEGÓRICAS
jogadores_fitted <- jogadores_clusters %>% 
  filter(Jogos > 1) %>% 
  dplyr::select(Jogador, Time, Comprador)

#CONSOLIDAÇÃO
valor_fitted <- valor_base %>% 
  dplyr::select(Valor) %>% 
  mutate(
    Jogador = jogadores_fitted$Jogador,
    Time = jogadores_fitted$Time,
    Comprador = jogadores_fitted$Comprador,
    BinomialZI = round(step_regressao_binomial_zeroinfl$fitted.values, 2),
    HurdleZI = round(step_regressao_hurdle_zeroinfl$fitted.values, 2),
    BinomialNeg = round(step_regressao_binomial_neg2$fitted.values, 2)) %>% 
  dplyr::select(Jogador, Time, Comprador, Valor, everything())

#EXPORTAÇÃO PARA ARQUIVO XLSX - COMPARATIVO DE VALORES
write_xlsx(valor_fitted, "valores_real_fitted.xlsx")

###DATAFRAME COM A COMPARAÇÃO ENTRE OS INDICADORES DOS 5 MODELOS
indicadores_zeroinfl <- bind_rows(indicadores_binomial_zeroinfl,
                                  indicadores_hurdle_zeroinfl,
                                  indicadores_binomial_neg)

#VISUALIZAÇÃO DOS INDICADORES
indicadores_zeroinfl %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)

################################################################################

################################################################################

###REGRESSÃO LOGÍSTICA BINÁRIA 
  #PROBABILIDADE DE SER VENDIDO OU NÃO POR VALOR > 0

#CRIAÇÃO DA BASE PARA MODELAGEM COM VARIÁVEL DEPENDENTE "VALOR" BINÁRIA
valor_base_logistica <- valor_base %>% 
  mutate(Valor = ifelse(Valor > 0,
                        yes = 1,
                        no = 0),
         Valor = factor(Valor)) -> valor_base_logistica

###MODELO LOGÍSTICO BINÁRIO
regressao_logistica_binaria <- glm(formula = Valor ~ . ,
                                   data = valor_base_logistica,
                                   family = "binomial")

#VISUALIZAÇÕES DO RESULTADO
  #QUI-QUADRADO: 430.2602
  #P-VALOR = 0
  #AIC: 466.6141
  #BIC: 692.6607
summary(regressao_logistica_binaria)
summ(model = regressao_logistica_binaria, confint = T, digits = 4, ci.width = 0.95)

#EXTRAÇÃO DO VALOR DE LOGLIKELIHOOD
  #LOGLIK = -185.3071
logLik(regressao_logistica_binaria)

#PROCEDIMENTO STEPWISE CORRIGIDO (5% DE SIGNIFICÂNCIA E 1 GRAU DE LIBERDADE)
step_regressao_logistica_binaria <- step(object = regressao_logistica_binaria,
                                         k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#VISUALIZAÇÕES DO RESULTADO APÓS PROCEDIMENTO STEPWISE
  #QUI-QUADRADO: 413.4245
  #P-VALOR = 0
  #AIC: 421.4498
  #BIC: 501.5080
#PREDITORAS (16):
  # Idade + Jogos + Minutos_media + Conversao + 
  # Toques_Area_media + Conducoes_media + Faltas_Recebidas_media + 
  # Interceptacoes_media + Cluster_Vendedor_1 + Cluster_Vendedor_2 + 
  # Cluster_Vendedor_4 + Cluster_Vendedor_5 + Posicao_Meia + 
  # Posicao_Volante + Serie_B + Serie_C
summary(step_regressao_logistica_binaria)
summ(model = step_regressao_logistica_binaria, confint = T, digits = 4, ci.width = 0.95)

#EXTRAÇÃO DO VALOR DE LOGLIKELIHOOD
  #LOGLIK = -193.7249
logLik(step_regressao_logistica_binaria)

#COMPARAÇÃO ENTRE OS DOIS MODELOS
  #P-VALOR > 0.5 (NÃO HÁ DIFERENÇA SIGNIFICATIVA ENTRE OS MODELOS)
lrtest(step_regressao_logistica_binaria, regressao_logistica_binaria)

#SALVAR MODELO PARA CARREGAMENTO POSTERIOR
saveRDS(step_regressao_logistica_binaria, file = "step_regressao_logistica_binaria.rds")

#MATRIZ DE CONFUSÃO - MODELO STEPWISE
  #CUTOFF: >=0.5
  #RESULTADOS: 
  #ACURÁCIA 90.85%
  #SENSITIVIDADE 70.06%
  #ESPECIFICIDADE 95.78%
confusionMatrix(
  table(predict(step_regressao_logistica_binaria, type = "response") >= 0.5, 
        valor_base_logistica$Valor == 1)[2:1, 2:1])

#CONSTRUÇÃO E PLOTAGEM DA CURVA ROC
  #RESULTADOS:
    #ÁREA ABAIXO DA CURVA: 94.1%
    #GINI: 88.3

curva_roc <- roc(response = valor_base_logistica$Valor, 
                 predictor = step_regressao_logistica_binaria$fitted.values)

ggroc(curva_roc, color = "darkorchid", size = 1) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
               color="orange", 
               linewidth = 0.2)+
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:", 
                     round(curva_roc$auc, 3), 
                     "|",
                     "Coeficiente de Gini", 
                     round((curva_roc$auc[1] - 0.5) / 0.5, 3))) +
  theme_bw()

################################################################################

################################################################################

###REGRESSÃO LOGÍSTICA MULTINOMIAL 
  #PROSPECÇÃO DE MERCADO

#SELEÇÃO DAS VARIÁVEIS
comprador_base <- jogadores_clusters %>% 
  filter(Jogos > 1) %>% 
  dplyr::select(46, 10:13, 16:21, 23:29, 31:35, 38:44)

dummies_multinomial <- valor_base %>% 
  dplyr::select(31:48)

#CONSOLIDAÇÃO DO ARQUIVO BASE
  #48 VARIÁVEIS (1 DEPENDENTE COM 6 CATEGORIAS E 47 PREDITORAS - SENDO 18 DUMMIES)
multinomial_base <- bind_cols(comprador_base, dummies_multinomial)

#FATORIZAÇÃO DA VARIÁVEL DEPENDENTE
multinomial_base$Cluster_Comprador <- as.factor(multinomial_base$Cluster_Comprador)

#SELEÇÃO DA CATEGORIA DE REFERÊNCIA (CLUSTER 3)
multinomial_base$Cluster_Comprador <- relevel(multinomial_base$Cluster_Comprador, 
                                              ref = "3")

#EXPORTAÇÃO PARA ARQUIVO XLSX
write_xlsx(multinomial_base, "multinomial_base.xlsx")

#REGRESSÃO MULTINOMIAL
regressao_multinomial <- multinom(formula = Cluster_Comprador ~ ., 
                                  data = multinomial_base)

#VISUALIZAÇÃO DO RESULTADO
summary(regressao_multinomial)

#PROCEDIMENTO STEPWISE CORRIGIDO (5% DE SIGNIFICÂNCIA E 1 GRAU DE LIBERDADE)
step_regressao_multinomial<- step(object = regressao_multinomial,
                                  k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#VISUALIZAÇÃO DO RESULTADO
  #VARIÁVEIS SELECIONADAS (23)
    # Idade, Minutos_media, xG_media, Conversao, 
    # Faltas_Recebidas_media, Assistencias, Passes_Chave_media, 
    # Passes_media, Passes_Progressivos_media, Passes_Area_media, 
    # Carrinhos_media, Bloqueios_media, Cluster_Vendedor_1, Cluster_Vendedor_2, 
    # Cluster_Vendedor_4, Cluster_Vendedor_5, Cluster_Vendedor_6, 
    # Posicao_Atacante, Posicao_Centroavante, Posicao_Lateral_Direito, 
    # Posicao_Lateral_Esquerdo, Posicao_Zagueiro, Janela_Inverno
summary(step_regressao_multinomial)

#EXTRAÇÃO DO VALOR DE LOGLIKELIHOOD
  #LOGLIK = -583.7385
logLik(step_regressao_multinomial)

#EXTRAÇÃO DO VALOR DE BIC (MUITO MELHOR QUE O POISSON)
  #BIC = 1972.594
cat(AIC(step_regressao_multinomial, k=log(dim(multinomial_base)[1])))

#EXTRAÇÃO DO VALOR DE AIC (MUITO MELHOR QUE O POISSON)
  #AIC = 1407.477
cat(AIC(step_regressao_multinomial))

#FUNÇÃO CHI-QUADRADO PARA EXTRAIR INDICADOR
qui <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

#ESTATÍSTICA GERAL DO MODELO
  #CHI-QUADRADO: 571.4246
  #P-VALOR < 0.05
qui(step_regressao_multinomial)

#INCLUSÃO DOS VALORES PREDITOS NA BASE DE DADOS
multinomial_base$preditos <- predict(step_regressao_multinomial, 
                                     newdata = multinomial_base, 
                                     type = "class")

#TABELA COMPARATIVA
multinomial_comparativo <- as.data.frame.matrix(table(multinomial_base$preditos, 
                                                      multinomial_base$Cluster_Comprador))

#EFICIÊNCIA GLOBAL
  #RESULTADO: 73%
multinomial_acuracia <- (round((sum(diag(table(multinomial_base$Cluster_Comprador, 
                                               multinomial_base$preditos))) / 
                                  sum(table(multinomial_base$Cluster_Comprador,
                                            multinomial_base$preditos))), 2))

#SALVAR MODELO PARA CARREGAMENTO POSTERIOR
saveRDS(step_regressao_multinomial, file = "step_regressao_multinomial.rds")

################################################################################
