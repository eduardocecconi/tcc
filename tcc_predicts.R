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

###CARREGAMENTO DOS MODELOS

zeroinfl <- readRDS("step_regressao_binomial_zeroinfl.rds")

logistica_binaria <- readRDS("step_regressao_logistica_binaria.rds")

logistica_multinomial <- readRDS("step_regressao_multinomial.rds")

################################################################################

#PREDICT BINOMIAL NEGATIVE ZERO-INFLATED
predict(zeroinfl, 
        data.frame(
          Idade = 20, #range de 16 a 41
          Jogos = 10, #range de 2 a 37
          Minutos_media = 45, #range de 8 a 102.67
          Gols = 2, #range de 0 a 18
          Conversao = 25, #range de 0 a 100
          Toques_Area_media = 3, #range de 0 a 6.56
          Dribles_media = 4, #range de 0 a 7.84
          Conducoes_media = 4, #range de 0 a 6.14
          Faltas_Recebidas_media = 3, #range de 0 a 7
          Passes_Terco_Final_media = 10, #range de 0 a 14.26
          Interceptacoes_media = 2, #range de 0 a 10.66
          Cluster_Vendedor_1 = 0, #viés negativo
          Cluster_Vendedor_2 = 0, #viés negativo
          Cluster_Vendedor_4 = 0, #viés negativo
          Cluster_Vendedor_5 = 0, #viés negativo
          Cluster_Vendedor_6 = 0,
          Posicao_Volante = 0,
          Posicao_Meia = 1,
          Serie_B = 0, #viés negativo
          Serie_C = 0 #viés negativo
        ),
        type = "response")

################################################################################

#PREDICT LOGÍSTICA BINÁRIA
predict(logistica_binaria, 
        data.frame(
          Idade = 20, #range de 16 a 41
          Jogos = 1, #range de 2 a 37
          Minutos_media = 45, #range de 8 a 102.67
          Conversao = 25, #range de 0 a 100
          Toques_Area_media = 3, #range de 0 a 6.56
          Conducoes_media = 4, #range de 0 a 6.14
          Faltas_Recebidas_media = 3, #range de 0 a 7
          Interceptacoes_media = 2, #range de 0 a 10.66
          Cluster_Vendedor_1 = 0, #viés negativo
          Cluster_Vendedor_2 = 0, #viés negativo
          Cluster_Vendedor_4 = 0, #viés negativo
          Cluster_Vendedor_5 = 0, #viés negativo
          Posicao_Meia = 0,
          Posicao_Volante = 0,
          Serie_B = 0, #viés negativo
          Serie_C = 0 #viés negativo
        ),
        type = "response")

################################################################################

#PREDICT LOGÍSTICA MULTINOMIAL
predict(logistica_multinomial, 
        data.frame(
          Idade = 20, #range de 16 a 41
          Minutos_media = 45, #range de 8 a 102.67
          xG_media = 0.5, #range de 0 a 1.31
          Conversao = 25, #range de 0 a 100
          Faltas_Recebidas_media = 3, #range de 0 a 7
          Assistencias = 1, #range de 0 a 12
          Passes_Chave_media = 1.5, #range de 0 a 4.26
          Passes_media = 25, #range de 0 a 79.3
          Passes_Progressivos_media = 8, #range de 0 a 12.67
          Passes_Area_media = 1.5, #range de 0 a 3.68
          Carrinhos_media = 1, #range de 0 a 2.65
          Bloqueios_media = 1, #range de 0 a 2.37
          Cluster_Vendedor_1 = 0, 
          Cluster_Vendedor_2 = 0, 
          Cluster_Vendedor_4 = 0, 
          Cluster_Vendedor_5 = 0, 
          Cluster_Vendedor_6 = 0,
          Posicao_Atacante = 0, 
          Posicao_Centroavante = 0, 
          Posicao_Lateral_Direito = 0,
          Posicao_Lateral_Esquerdo = 0, 
          Posicao_Zagueiro = 0, 
          Janela_Inverno = 0
        ), 
        type = "probs")

################################################################################

###CARREGAR ARQUIVOS PARA VISUALIZAÇÕES
#COM CLUSTERS DE COMPRADOR E VENDEDOR
valor_fitted <- read_excel("valores_real_fitted.xlsx") 

###VISUALIZAÇÃO
#PERMITE DIVERSAS CUSTOMIZAÇÕES PELA FUNÇÃO FILTER
visualizacao_fitted <- valor_fitted %>% 
  arrange(desc(Valor)) %>% 
  mutate(Linha = 1:820) %>% 
  dplyr::filter(Time == "Palmeiras" & Valor != 0) %>% 
  dplyr::select(1:5)

visualizacao_fitted %>%   
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)

################################################################################

