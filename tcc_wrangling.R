library(tidyverse)
library(readxl)
library(writexl)
library(stats)
library(reshape2)
library(scales)

#FUNÇÃO PARA EVITAR NOTAÇÃO CIENTÍFICA
options(scipen=999)

###CARREGAR DADOS CONSOLIDADOS DE TRANSFERÊNCIAS
###SÉRIES A E B - TEMPORADAS 2018 A 2022

transferencias_2018 <- read_excel("brasil_transferencias_2018.xlsx")

transferencias_2019 <- read_excel("brasil_transferencias_2019.xlsx")

transferencias_2020 <- read_excel("brasil_transferencias_2020.xlsx")

transferencias_2021 <- read_excel("brasil_transferencias_2021.xlsx")

transferencias_2022 <- read_excel("brasil_transferencias_2022.xlsx")

###BANCO DE DADOS RAIZ
#946 OBSERVAÇÕES E 55 VARIÁVEIS
#190 DAS 946 OBSERVAÇÕES COM VALORES > 0 EM TRANSFERÊNCIAS (20%)
transferencias_brasil_2018_2022 <- bind_rows(transferencias_2018, transferencias_2019, 
                                  transferencias_2020, transferencias_2021, 
                                  transferencias_2022) %>% 
  mutate(Idade = as.numeric(Idade),
         across(12:55, replace_na, 0))

#AJUSTE DE NOMES DE 47 PAÍSES COMPRADORES
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Azerbaijan"] <- 'Azerbaijão'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Brazil"] <- 'Brasil'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Spain"] <- 'Espanha'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Qatar"] <- 'Catar'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Saudi Arabia"] <- 'Arábia Saudita'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Australia"] <- 'Austrália'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Russia"] <- 'Rússia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Italy"] <- 'Itália'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "United States"] <- 'Estados Unidos'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Thailand"] <- 'Tailândia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "England"] <- 'Inglaterra'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Mexico"] <- 'México'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Japan"] <- 'Japão'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Paraguay"] <- 'Paraguai'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Turkey"] <- 'Turquia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Colombia"] <- 'Colômbia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Bulgaria"] <- 'Bulgária'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Denmark"] <- 'Dinamarca'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Egypt"] <- 'Egito'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Germany"] <- 'Alemanha'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Romania"] <- 'Romênia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Ukraine"] <- 'Ucrânia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Latvia"] <- 'Letônia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Greece"] <- 'Grécia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Canada"] <- 'Canadá'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "France"] <- 'França'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Armenia"] <- 'Armênia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "United Arab Emirates"] <- 'Emirados Árabes'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Belgium"] <- 'Bélgica'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Bahrain"] <- 'Bahrein'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Bolivia"] <- 'Bolívia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Cyprus"] <- 'Chipre'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Lithuania"] <- 'Lituânia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Moldova"] <- 'Moldávia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Switzerland"] <- 'Suíça'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Dominican Republic"] <- 'República Dominicana'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Netherlands"] <- 'Holanda'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Kazakhstan"] <- 'Cazaquistão'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Korea, South"] <- 'Coréia do Sul'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Malaysia"] <- 'Malásia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Hongkong"] <- 'Hong Kong'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Monaco"] <- 'França'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Croatia"] <- 'Croácia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Uruguay"] <- 'Uruguai'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Hungary"] <- 'Hungria'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Poland"] <- 'Polônia'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "Ecuador"] <- 'Equador'
transferencias_brasil_2018_2022$Pais_Comprador[transferencias_brasil_2018_2022$Pais_Comprador %in% "New Zealand"] <- 'Nova Zelândia'

#AJUSTE DE NOMES DE 5 POSIÇÕES (EXCLUSÃO DE HÍFENS)
transferencias_brasil_2018_2022$Posicao[transferencias_brasil_2018_2022$Posicao %in% "Meia-Atacante"] <- 'Meia_Atacante'
transferencias_brasil_2018_2022$Posicao[transferencias_brasil_2018_2022$Posicao %in% "Extremo-Direito"] <- 'Extremo_Direito'
transferencias_brasil_2018_2022$Posicao[transferencias_brasil_2018_2022$Posicao %in% "Extremo-Esquerdo"] <- 'Extremo_Esquerdo'
transferencias_brasil_2018_2022$Posicao[transferencias_brasil_2018_2022$Posicao %in% "Lateral-Direito"] <- 'Lateral_Direito'
transferencias_brasil_2018_2022$Posicao[transferencias_brasil_2018_2022$Posicao %in% "Lateral-Esquerdo"] <- 'Lateral_Esquerdo'

#SUBSTITUIÇÃO DE NA'S NA VARIÁVEL "LIGA"
transferencias_brasil_2018_2022$Liga[is.na(transferencias_brasil_2018_2022$Liga)] <- 'Sem'

###########################       JOGADORES      ###############################   

#ORGANIZAÇÃO DAS VARIÁVEIS PARA ESTUDO DAS CORRELAÇÕES
transferencias_brasil_2018_2022 <- transferencias_brasil_2018_2022 %>% 
  dplyr::select(Nome, Jogador, Time, Posicao, Liga, Temporada, Janela, 
                Comprador, Pais_Comprador, Valor, Idade, Jogos, everything())

#ESTUDO DAS CORRELAÇÕES
jogadores_correl <- rcorr(as.matrix(transferencias_brasil_2018_2022[,10:55]), type="pearson")
jogadores_matriz <- jogadores_correl$r

###CONSTRUÇÃO DO ARQUIVO-BASE PARA MODELAGEM
#RESULTADO: 861 OBSERVAÇÕES E 44 VARIÁVEIS (36 NUMÉRICAS)
#699 DAS 861 OBSERVAÇÕES COM VALOR = 0 (81.2%)
  #EXCLUSÃO DE GOLEIROS: 65 OBSERVAÇÕES (8 COM VALOR)
  #EXCLUSÃO DE JOGADORES SEM DADOS DE DESEMPENHO (20, TODOS COM VALOR)
  #EXCLUSÃO DE 4 VARIÁVEIS DEVIDO A CORRELAÇÕES ACIMA DE 0.9
    #MINUTOS (0.92 CORREL COM JOGOS)
    #GOLS_SEM_PENALTI (0.97 CORREL COM GOLS)
    #ACOES_ATAQUE_MEDIA (0.92 CORREL COM DRILES_MEDIA)
    #PASSES_RECEBIDOS_MEDIA (0.97 CORREL COM PASSES_MEDIA)
  #EXCLUSÃO DE 6 VARIÁVEIS ESPECÍFICAS DE GOLEIROS
    #GOLS_CONCEDIDOS, GOLS_CONCEDIDOS_MEDIA, GOLS_EVITADOS_MEDIA,
    #JOGOS_INVICTO, PERCENTUAL_DEFESAS, PASSES_RECEIDOS_GOLEIRO, 
  #EXCLUSÃO DA VARIÁVEL "TEMPORADA" (NÃO PERMITIRIA EXTRAPOLAÇÃO NAS PREVISÕES)

jogadores <- transferencias_brasil_2018_2022 %>% 
  filter(Posicao != "Goleiro" & Jogos > 0) %>% 
  rename(Distancia_Passes = Distância_passes) %>% 
  dplyr::select(everything(), -6, -13, -18, -22, -33, -50, -51, -52, -53, -54, -55)

################# VISUALIZAÇÃO DOS JOGADORES EXCLUÍDOS SEM JOGOS  ###############
jogadores_semjogos <- transferencias_brasil_2018_2022 %>%  
  filter(Jogos == 0 & Posicao != "Goleiro") %>% 
  dplyr::select(1:3, 4:11)

#MÉDIA DE VALOR ENTRE OS 20 JOGADORES SEM DADOS - € 3.153.250
summary(jogadores_semjogos$Valor)

#MÉDIA DE IDADE ENTRE OS 20 JOGADORES SEM DADOS - 19.9 ANOS
summary(jogadores_semjogos$Idade)

#COMPRADORES MAIS FREQUENTES NESTES CASOS
#SHAKTHAR - 4 DE 20, INCLUINDO A MAIOR
#MAN. CITY - 2 COMPRAS (3º E 4º MAIORES VALORES, AMBOS 18 ANOS)
#BOAVISTA - 2 COMPRAS
table(jogadores_semjogos$Comprador)

#VENDEDORES MAIS FREQUENTES NESTES CASOS
#PALMEIRAS - 4 DE 20, INCLUINDO A MAIOR
#SÃO PAULO - 3
#GRÊMIO, SANTOS E FLAMENGO - 2 CADA
table(jogadores_semjogos$Time)

#EXPORTAÇÃO PARA ARQUIVO XLSX
write_xlsx(jogadores_semjogos, "jogadores_semjogos.xlsx")

###################    VISUALIZAÇÃO DOS GOLEIROS EXCLUÍDOS   ###################
goleiros <- transferencias_brasil_2018_2022 %>%  
  filter(Posicao == "Goleiro") %>% 
  dplyr::select(1:3, 4:12, 14, 32, 50:54)

#MÉDIA DE VALOR ENTRE OS 65 GOLEIROS (APENAS 8 COM VALORES) - € 302.308
summary(goleiros$Valor)

#MÉDIA DE IDADE ENTRE OS 65 GOLEIROS - 29.5 ANOS
summary(goleiros$Idade)

#COMPRADORES MAIS FREQUENTES NESTES CASOS
#ATLÉTICO-GO, INTERNACIONAL, CSA E RB BRAGANTINO - 3 CADA
table(goleiros$Comprador)

#VENDEDORES MAIS FREQUENTES NESTES CASOS
#ATLÉTICO-GO, INTERNACIONAL, CSA E RB BRAGANTINO - 3 CADA
table(goleiros$Time)
#CSA - 6
#VASCO E CRUZEIRO - 4 CADA

#EXPORTAÇÃO PARA ARQUIVO XLSX
write_xlsx(goleiros, "goleiros.xlsx")

################################################################################

###RESUMO DE TOTAL DE TRANSFERÊNCIAS E TOTAL DE VALOR ARRECADADO POR TIME
#TABELA DE FREQUÊNCIA TIME x JANELA
tabela_frequencia_time_janela <- table(jogadores$Time, jogadores$Janela)

tabela_frequencia_time_janela <- as.data.frame(tabela_frequencia_time_janela) %>%
  rename(Time = 1,
         Janela = 2,
         Total = 3) %>%
  dcast(Time ~ Janela, value.var="Total", fun.aggregate=sum)

#CONTAGEM DO TOTAL DE TRANSFERÊNCIAS POR TIME
transferencias_time <- tabela_frequencia_time_janela %>% 
  mutate("Total de Transferências" = `Inverno` + `Verão`) %>% 
  dplyr::select(Time, `Total de Transferências`)

#SOMA DO VALOR DAS TRANSFERÊNCIAS POR TIME
valores <- jogadores %>% 
  group_by(Time) %>% 
  summarise("Valor Arrecadado"=sum(Valor),
            "Média de Idade"=round(mean(Idade), 2)) %>% 
  ungroup()

#RESUMO CONSOLIDADO - TRANSFERÊNCIAS E VALORES POR TIME
resumo_transferencias_time <- left_join(valores, transferencias_time) %>% 
  dplyr::select(Time, `Total de Transferências`, `Valor Arrecadado`, `Média de Idade`) %>% 
  arrange(desc(`Valor Arrecadado`))

#TABELA DE FREQUÊNCIAS JANELA x PAÍS COMPRADOR
tabela_frequencia_janela_comprador <- table(jogadores$Pais_Comprador, jogadores$Janela)

tabela_frequencia_janela_comprador <- as.data.frame(tabela_frequencia_janela_comprador) %>%
  rename(`País` = 1,
         Janela = 2,
         Total = 3) %>%
  dcast(`País` ~ Janela, value.var="Total", fun.aggregate=sum)

#CONTAGEM DO TOTAL DE TRANSFERÊNCIAS POR PAÍS COMPRADOR
transferencias_comprador <- tabela_frequencia_janela_comprador %>% 
  mutate("Total de Transferências" = Inverno + `Verão`) %>% 
  rename("Transferências Inverno" = Inverno,
         "Transferências Verão" = `Verão`) %>% 
  dplyr::select(`País`, `Total de Transferências`, `Transferências Inverno`, `Transferências Verão`)

#SOMA DOS VALORES GASTOS POR PAÍS COMPRADOR
valores_comprador <- jogadores %>% 
  group_by(Pais_Comprador) %>% 
  summarise("Valor Total" = sum(Valor),
            "Média de Idade" = mean(Idade)) %>% 
  rename("País" = Pais_Comprador) %>% 
  dplyr::select(`País`, `Valor Total`, `Média de Idade`) %>% 
  arrange(desc(`Valor Total`))

#SOMA DOS VALORES GASTOS POR JANELA
valores_janela_verao <- jogadores %>% 
  filter(Janela == "Verão") %>% 
  group_by(Pais_Comprador) %>% 
  summarise("Valor Verão" = sum(Valor)) %>% 
  rename("País" = Pais_Comprador) %>% 
  dplyr::select(`País`, `Valor Verão`) %>% 
  arrange(desc(`Valor Verão`))

valores_janela_inverno <- jogadores %>% 
  filter(Janela == "Inverno") %>% 
  group_by(Pais_Comprador) %>% 
  summarise("Valor Inverno" = sum(Valor)) %>% 
  rename("País" = Pais_Comprador) %>% 
  dplyr::select(`País`, `Valor Inverno`) %>% 
  arrange(desc(`Valor Inverno`))

#RESUMO CONSOLIDADO - TRANSFERÊNCIAS E VALORES POR PAÍS COMPRADOR
resumo_transferencias_comprador <- left_join(valores_comprador, transferencias_comprador) %>% 
  dplyr::select(`País`, `Valor Total`, `Total de Transferências`, 
         `Transferências Inverno`, `Transferências Verão`, `Média de Idade`) %>% 
  arrange(desc(`Valor Total`))

#EXPORTAÇÃO PARA ARQUIVO XLSX - TRANSFERENCIAS 2018-2022
write_xlsx(jogadores, "jogadores.xlsx")

#EXPORTAÇÃO PARA ARQUIVO XLSX - RESUMO TIMES
write_xlsx(resumo_transferencias_time, "resumo_transferencias_time.xlsx")

#EXPORTAÇÃO PARA ARQUIVO XLSX - RESUMO COMPRADOR
write_xlsx(resumo_transferencias_comprador, "resumo_transferencias_comprador.xlsx")

###VISUALIZAÇÕES
#HISTOGRAMA DE TODAS AS TRANSFERÊNCIAS
ggplot(jogadores, aes(x = Valor)) +
  geom_histogram(fill = "black", bins = 50) +
  labs(title = "Valores de transferências de jogadores",
       subtitle = "Campeonatos brasileiros (todas as divisões)",
       caption = "Período: 2018 a 2021",
       x = "Valor",
       y = "Frequência") +
  scale_x_continuous(labels  = 
                       label_number(scale = 1e-6, prefix = "€", suffix = "M", accuracy = 1)) +
  theme_bw()

#HISTOGRAMA DOS TOTAIS AGRUPADOS POR CLUBE VENDEDOR
ggplot(resumo_transferencias_time, aes(x = `Valor Arrecadado`)) +
  geom_histogram(fill = "black", bins = 50) +
  labs(title = "Valores de transferências de jogadores",
       subtitle = "Campeonatos brasileiros (todas as divisões)",
       caption = "Período: 2018 a 2022",
       x = "Valor total arrecadado por clube",
       y = "Frequência") +
  scale_x_continuous(labels  = 
                       label_number(scale = 1e-6, prefix = "€", suffix = "M", accuracy = 1)) +
  theme_bw()

#HISTOGRAMA DOS TOTAIS AGRUPADOS POR COMPRADOR
ggplot(resumo_transferencias_comprador, aes(x = `Valor Total`)) +
  geom_histogram(fill = "black", bins = 50) +
  labs(title = "Valores de transferências de jogadores",
       subtitle = "Campeonatos brasileiros (todas as divisões)",
       caption = "Período: 2018 a 2022",
       x = "Valor total investido por país",
       y = "Frequência") +
  scale_x_continuous(labels  = 
                       label_number(scale = 1e-6, prefix = "€", suffix = "M", accuracy = 1)) +
  theme_bw()
