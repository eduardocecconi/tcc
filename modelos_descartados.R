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

###MODELO DE REGRESSÃO LINEAR MÚLTIPLA - JOGADORES
regressao_linear <- lm(formula = Valor ~ . , data = valor_base)

#VISUALIZAÇÃO DO RESULTADO
# R^2: 35.01%
summary(regressao_linear)

#DIAGNÓSTICO DE MULTICOLINEARIDADE
#NÃO HÁ MULTICOLINEARIDADE - NENHUM VALOR DE VIF > 10
ols_vif_tol(regressao_linear)

#PROCEDIMENTO STEPWISE CORRIGIDO (5% DE SIGNIFICÂNCIA E 1 GRAU DE LIBERDADE)
step_regressao_linear <- step(regressao_linear, k = 3.841459)

#VISUALIZAÇÃO DO RESULTADO
# R^2: 33.5%  (INFERIOR AO MODELO ANTERIOR)
#PREDITORAS (13): 
# Idade + Jogos + Minutos_media + Gols + xG_media + 
# Chutes_media + Toques_Area_media + Passes_Terco_Final_media + 
# Cluster_Vendedor_1 + Cluster_Vendedor_2 + Cluster_Vendedor_4 + 
# Cluster_Vendedor_6 + Posicao_Centroavante
summary(step_regressao_linear)

#TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE
#Resultado: p-valor < 0.05,
#Não houve aderência dos resíduos à normalidade
sf.test(step_regressao_linear$residuals)

#DIAGNÓSTICO DE HETEROCEDASTICIDADE
#Resultado: p-valor < 0.05,
#A variância dos erros não é constante (indica omissão de variável relevante)
ols_test_breusch_pagan(step_regressao_linear)

#PLOTAGEM
#COMPARAÇÃO DA DISTRIBUIÇÃO DOS RESÍDUOS COM UMA CURVA NORMAL TEÓRICA
valor_base %>%
  mutate(residuos = step_regressao_linear$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = after_stat(density)), 
                 color = "orange", 
                 fill = "orange", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_regressao_linear$residuals),
                            sd = sd(step_regressao_linear$residuals)),
                aes(color = "Curva Normal Teórica"),
                linewidth = 1) +
  scale_color_manual("Legenda:",
                     values = "black") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

################################################################################

###TRANSFORMAÇÃO DE BOX-COX - REGRESSÃO NÃO-LINEAR MÚLTIPLA
# TRANSFORMAÇÃO DOS ZEROS EM VALORES CONTÍNUOS
valor_boxcox <- valor_base %>% 
  mutate(Valor2 = ifelse(valor_base$Valor == 0, 0.01, valor_base$Valor)) %>% 
  dplyr::select(Valor2, everything(), -Valor)

#CÁLCULO DO LAMBDA DA VARIÁVEL "VALOR"
#RESULTADO: 
#LAMBDA = -0.2605941 (DISTRIBUIÇÃO SEM PADRÃO)
lambda_boxcox <- powerTransform(valor_boxcox$Valor2)
lambda_boxcox

#INCLUSÃO DA VARIÁVEL "VALOR" TRANSFORMADA
valor_boxcox$Valor_boxcox <- (((valor_boxcox$Valor2 ^ lambda_boxcox$lambda) - 1) / 
                                lambda_boxcox$lambda)

#MODELO COM VARIÁVEL DEPENDENTE TRANSFORMADA
regressao_boxcox <- lm(formula = Valor_boxcox ~ . -Valor2, data = valor_boxcox)

#VISUALIZAÇÃO DO RESULTADO
# R^2: 44.95%
summary(regressao_boxcox)

#PROCEDIMENTO STEPWISE CORRIGIDO (5% DE SIGNIFICÂNCIA E 1 GRAU DE LIBERDADE)
step_regressao_boxcox <- step(regressao_boxcox, k = 3.841459)

#VISUALIZAÇÃO DO RESULTADO
# R^2: 42.54% 
#PREDITORAS (14):
# Idade + Minutos_media + Passes_Profundidade_media + 
# Bloqueios_media + Vermelhos + Cluster_Vendedor_1 + Cluster_Vendedor_2 + 
# Cluster_Vendedor_4 + Cluster_Vendedor_5 + Cluster_Vendedor_6 + 
# Posicao_Extremo_Direito + Posicao_Lateral_Direito + Posicao_Meia_Atacante + 
# Serie_B
summary(step_regressao_boxcox)

#TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE
#Resultado: p-valor < 0.05,
#Também não apresentou aderência à normalidade
sf.test(step_regressao_boxcox$residuals)

#DIAGNÓSTICO DE HETEROCEDASTICIDADE
#Resultado: p-valor < 0.05,
#A variância dos erros não é constante (indica omissão de variável relevante)
ols_test_breusch_pagan(step_regressao_boxcox)

#COMPARAÇÃO DA DISTRIBUIÇÃO DOS RESÍDUOS COM UMA CURVA NORMAL TEÓRICA
valor_base %>%
  mutate(residuos = step_regressao_boxcox$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = after_stat(density)), 
                 color = "orange", 
                 fill = "orange", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_regressao_boxcox$residuals),
                            sd = sd(step_regressao_boxcox$residuals)),
                aes(color = "Curva Normal Teórica"),
                linewidth = 1) +
  scale_color_manual("Legenda:",
                     values = "black") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

################################################################################

###MODELO POISSON PARA DADOS DE CONTAGEM
regressao_poisson <- glm(formula = Valor ~ . , 
                         data = valor_base, 
                         family = poisson)

#VISUALIZAÇÃO DO RESULTADO
#TODAS AS VARIÁVEIS COM O MESMO P-VALOR (TORNA INÚTIL O PROCEDIMENTO STEPWISE)
#STANDARD ERRORS PRÓXIMOS A ZERO - INDICANDO SEPARAÇÃO PERFEITA
summary(regressao_poisson) 

#DIAGNÓSTICO DE MULTICOLINEARIDADE
#VALORES DE VIF > 10 EM 6 VARIÁVEIS 
#PASSES_PROGRESSIVOS, PASSES_TERCO_FINAL, PASSES_AREA, PASSES, DRIBLES, TOQUES_AREA
vif(regressao_poisson)

#EXTRAÇÃO DO VALOR DE LOGLIKELIHOOD
#LOGLIK = -347935401
logLik(regressao_poisson)

#EXTRAÇÃO DO VALOR DE BIC
#BIC = 695871124
cat(AIC(regressao_poisson, k=log(dim(valor_base)[1])))

#EXTRAÇÃO DO VALOR DE AIC
#AIC = 695870898
cat(AIC(regressao_poisson))

#DATAFRAME COM OS 3 INDICADORES
indicadores_poisson <- data.frame(
  Modelo = "Poisson",
  LogLik = -347935401,
  BIC =  695871124,
  AIC = 695870898, 
  row.names = NULL)

#VISUALIZAÇÃO
indicadores_poisson %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)

#LIKELIHOOD RATIO TEST PARA COMPARAÇÃO DO RESULTADO COM MODELO NULO
#Modelo estimado obteve melhor LogLik que o simulado
#p-valor do chi-quadrado < 0.05: diferença entre os modelos é significativa
lrtest(regressao_poisson)

################################################################################

###MODELO POISSON ZERO INFLATED
regressao_poisson_zeroinfl <- zeroinfl(Valor ~ . | . , 
                                       data = valor_base,
                                       dist = "poisson",
                                       link = "logit")

#VISUALIZAÇÃO DO RESULTADO
#TODAS AS VARIÁVEIS COM MESMO P-VALOR NO MODELO COUNT
summary(regressao_poisson_zeroinfl)

#PROCEDIMENTO STEPWISE (5% DE SIGNIFICÂNCIA)
step_regressao_poisson_zeroinfl <- be.zeroinfl(regressao_poisson_zeroinfl, 
                                               data=valor_base, 
                                               dist="poisson", 
                                               alpha=0.05, 
                                               trace=FALSE)

#VISUALIZAÇÃO DO RESULTADO
#TODAS AS 47 VARIÁVEIS MANTIDAS NO MODELO COUNT
#16 VARIÁVEIS MANTIDAS NO MODELO ZERO-INFLATED
# Idade + Jogos + Minutos_media + Conversao + Toques_Area_media + 
# Conducoes_media + Faltas_Recebidas_media + Interceptacoes_media + 
# Cluster_Vendedor_1 + Cluster_Vendedor_2 + Cluster_Vendedor_4 + 
# Cluster_Vendedor_5 + Posicao_Meia + Posicao_Volante + 
# Serie_B + Serie_C
summary(step_regressao_poisson_zeroinfl)

#EXTRAÇÃO DO VALOR DE LOGLIKELIHOOD
#LOGLIK = -113512916
logLik(step_regressao_poisson_zeroinfl)

#EXTRAÇÃO DO VALOR DE BIC
#BIC = 227026268
cat(AIC(step_regressao_poisson_zeroinfl, k=log(dim(valor_base)[1])))

#EXTRAÇÃO DO VALOR DE AIC
#AIC = 227025961
cat(AIC(step_regressao_poisson_zeroinfl))

#DATAFRAME COM OS 3 INDICADORES
indicadores_poisson_zeroinfl <- data.frame(
  Modelo = "PoissonZI",
  LogLik = -113512916,
  BIC =  227026268,
  AIC = 227025961, 
  row.names = NULL)

#VISUALIZAÇÃO
indicadores_poisson_zeroinfl %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)

#LIKELIHOOD RATIO TEST PARA COMPARAÇÃO DO RESULTADO COM MODELO SIMULADO
#MODELO SEM STEPWISE SUPERIOR AO MODELO NULO
#EXISTE DIFERENÇA SIGNIFICATIVA ENTRE OS MODELOS ESTIMADO E NULO
lrtest(regressao_poisson_zeroinfl)

#LIKELIHOOD RATIO TEST POISSON ZERO-INFLATED E POISSON
#Modelo Zero-Inflated obteve melhor LogLik
#p-valor do chi-quadrado indica diferença significativa em relação ao modelo Poisson
lrtest(regressao_poisson, regressao_poisson_zeroinfl)

#TESTE DE VUONG
#p-valor do teste de Likelihood < 0.05 (MODELO STEP ZEROINFL TEM MELHOR AJUSTE)
#p-valor do teste de variância  = 0.05 (MODELOS NÃO SÃO DISTINGUÍVEIS)
vuongtest(step_regressao_poisson_zeroinfl, regressao_poisson, 
          nested = FALSE,
          adj = "aic")

################################################################################


################################################################################

###REGRESSÃO LINEAR MÚLTIPLA APENAS DE OBSERVAÇÕES COM VALOR > 0
#ALTERNATIVA PARA IMPLEMENTAÇÃO EM DADOS INTERNOS DE CLUBES
#157 OBSERVAÇÕES
valor_base_comvalor <- valor_base %>% 
  filter(Valor != 0) 

#MODELO LINEAR MÚLTIPLO - COM VALOR
regressao_comvalor <- lm(formula = Valor ~ . , data = valor_base_comvalor)

#VISUALIZAÇÃO DO RESULTADO
# R^2: 61.6%
summary(regressao_comvalor)

#DIAGNÓSTICO DE MULTICOLINEARIDADE
#3 VARIÁVEIS COM VIF > 10
#PASSES_PROGRESSIVOS, PASSES_PROFUNDIDADE, PASSES_AREA
ols_vif_tol(regressao_comvalor)

#PROCEDIMENTO STEPWISE CORRIGIDO (5% DE SIGNIFICÂNCIA E 1 GRAU DE LIBERDADE)
step_regressao_comvalor <- step(regressao_comvalor, k = 3.841459)

#VISUALIZAÇÃO DO RESULTADO
summary(step_regressao_comvalor)

#TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE
#Resultado: p-valor < 0.05,
#Não houve aderência dos resíduos à normalidade
sf.test(step_regressao_comvalor$residuals)

#DIAGNÓSTICO DE HETEROCEDASTICIDADE
#Resultado: p-valor < 0.05,
#A variância dos erros não é constante (indica omissão de variável relevante)
ols_test_breusch_pagan(step_regressao_comvalor)

#COMPARAÇÃO DA DISTRIBUIÇÃO DOS RESÍDUOS COM UMA CURVA NORMAL TEÓRICA
valor_base_comvalor %>%
  mutate(residuos = step_regressao_comvalor$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = after_stat(density)), 
                 color = "bisque4", 
                 fill = "orange", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_regressao_comvalor$residuals),
                            sd = sd(step_regressao_comvalor$residuals)),
                aes(color = "Curva Normal Teórica"),
                linewidth = 1) +
  scale_color_manual("Legenda:",
                     values = "darkorchid") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

################################################################################

###TRANSFORMAÇÃO DE BOX-COX - REGRESSÃO NÃO-LINEAR MÚLTIPLA
# TRANSFORMAÇÃO DOS ZEROS EM VALORES CONTÍNUOS
valor_boxcox_comvalor <- valor_base_comvalor %>% 
  mutate(Valor2 = ifelse(valor_base_comvalor$Valor == 0, 0.01, valor_base_comvalor$Valor)) %>% 
  dplyr::select(Valor2, everything(), -Valor)

#CÁLCULO DO LAMBDA DA VARIÁVEL "VALOR"
#RESULTADO: 
#LAMBDA = 0.05489084 (DISTRIBUIÇÃO SEM PADRÃO)
lambda_boxcox_comvalor <- powerTransform(valor_boxcox_comvalor$Valor2)
lambda_boxcox_comvalor

#INCLUSÃO DA VARIÁVEL "VALOR" TRANSFORMADA
valor_boxcox_comvalor$Valor_boxcox <- (
  ((valor_boxcox_comvalor$Valor2 ^ lambda_boxcox_comvalor$lambda) - 1) / 
    lambda_boxcox_comvalor$lambda)

#MODELO COM VARIÁVEL DEPENDENTE TRANSFORMADA
regressao_boxcox_comvalor <- lm(formula = Valor_boxcox ~ . -Valor2, data = valor_boxcox_comvalor)

#VISUALIZAÇÃO DO RESULTADO
# R^2: 66.77%
summary(regressao_boxcox_comvalor)

#PROCEDIMENTO STEPWISE CORRIGIDO (5% DE SIGNIFICÂNCIA E 1 GRAU DE LIBERDADE)
step_regressao_boxcox_comvalor <- step(regressao_boxcox_comvalor, k = 3.841459)

#VISUALIZAÇÃO DO RESULTADO
# R^2: 62.13% 
#PREDITORAS (11):
# Idade + Jogos + Minutos_media + Gols + 
# Dribles_media + Pre_Assistencia_media + Passes_Terco_Final_media + 
# Cluster_Vendedor_1 + Cluster_Vendedor_2 + Cluster_Vendedor_4 + 
# Cluster_Vendedor_6
summary(step_regressao_boxcox_comvalor)

#TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE
#Resultado: p-valor < 0.05,
#Também não apresentou aderência à normalidade
sf.test(step_regressao_boxcox_comvalor$residuals)

#DIAGNÓSTICO DE HETEROCEDASTICIDADE
#Resultado: p-valor > 0.05 (0.56),
#A variância dos erros é constante (HOMOCEDASTICIDADE)
ols_test_breusch_pagan(step_regressao_boxcox_comvalor)

#COMPARAÇÃO DA DISTRIBUIÇÃO DOS RESÍDUOS COM UMA CURVA NORMAL TEÓRICA
valor_base_comvalor %>%
  mutate(residuos = step_regressao_boxcox_comvalor$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = after_stat(density)), 
                 color = "orange", 
                 fill = "orange", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_regressao_boxcox_comvalor$residuals),
                            sd = sd(step_regressao_boxcox_comvalor$residuals)),
                aes(color = "Curva Normal Teórica"),
                linewidth = 1) +
  scale_color_manual("Legenda:",
                     values = "black") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()
