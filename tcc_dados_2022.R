library(tidyverse)
library(readxl)
library(writexl)
library(stats)

###DADOS DE TRANSFERÊNCIAS - 2022 - SÉRIES A E B

#CARREGAR LISTAS DE TRANSFERÊNCIAS
transf_seriea_22 <- read_excel("tm_transferencias_seriea_2022.xlsx")

transf_serieb_22 <- read_excel("tm_transferencias_serieb_2022.xlsx")

###SUBSTITUIR NA'S NAS VARIÁVEIS "LEAGUE" E "COUNTRY"
transf_seriea_22$league[is.na(transf_seriea_22$league)] <- 'Série A'
transf_seriea_22$country[is.na(transf_seriea_22$country)] <- 'Brazil'

transf_serieb_22$league[is.na(transf_serieb_22$league)] <- 'Série B'
transf_serieb_22$country[is.na(transf_serieb_22$country)] <- 'Brazil'

#CARREGAR BIOGRAFIA DOS JOGADORES
jogadores_seriea_22 <- read_excel("tm_jogadores_seriea_2022.xlsx")

jogadores_serieb_22 <- read_excel("tm_jogadores_serieb_2022.xlsx")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA NAME_IN_HOME_COUNTRY
jogadores_seriea_22$full_name <- ifelse(is.na(jogadores_seriea_22$full_name), 
                                        jogadores_seriea_22$name_in_home_country, 
                                        jogadores_seriea_22$full_name)

jogadores_serieb_22$full_name <- ifelse(is.na(jogadores_serieb_22$full_name), 
                                        jogadores_serieb_22$name_in_home_country, 
                                        jogadores_serieb_22$full_name)

#SELECIONAR O NOME DO JOGADOR NA COLUNA PLAYER_NAME
jogadores_seriea_22$player_name <- word(jogadores_seriea_22$player_name, -2, -1)

jogadores_serieb_22$player_name <- word(jogadores_serieb_22$player_name, -2, -1)

#EXCLUIR ESPAÇOS NO INÍCIO DO NOME DOS JOGADORES
jogadores_seriea_22$player_name <- str_trim(jogadores_seriea_22$player_name, side = "both")

jogadores_serieb_22$player_name <- str_trim(jogadores_serieb_22$player_name, side = "both")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA PLAYER_NAME
jogadores_seriea_22 <- jogadores_seriea_22 %>% 
  mutate(full_name = ifelse(is.na(jogadores_seriea_22$full_name), 
                            jogadores_seriea_22$player_name, jogadores_seriea_22$full_name)) %>%
  select(player_name, full_name)

jogadores_serieb_22 <- jogadores_serieb_22 %>% 
  mutate(full_name = ifelse(is.na(jogadores_serieb_22$full_name), 
                            jogadores_serieb_22$player_name, jogadores_serieb_22$full_name)) %>%
  select(player_name, full_name)

#SELECIONAR A COMPETIÇÃO NA COLUNA LEAGUE DO BANCO DE TRANSFERÊNCIAS
transf_seriea_22$league <- word(transf_seriea_22$league, -2, -1)

transf_serieb_22$league <- word(transf_serieb_22$league, -2, -1)

###WRANGLING SÉRIE A 2022
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_seriea_22 <- left_join(transf_seriea_22,jogadores_seriea_22)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_seriea_22 <- distinct(transf_seriea_22, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2022 SÉRIE A
#REDUÇÃO DE 616 PARA 102 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_22_seriea <- transf_seriea_22 %>%
  select(player_name, full_name, player_age, player_position, league,
         season, transfer_type, is_loan, transfer_fee, 
         window, team_name, club_2, league_2, country_2) %>%
  filter(is_loan == FALSE & transfer_fee >= 0 & transfer_type == "Departures") %>%
  rename(Jogador = player_name,
         Nome = full_name,
         Idade = player_age,
         Posicao = player_position,
         Liga = league,
         Temporada = season,
         Janela = window,
         Valor = transfer_fee,
         Time = team_name,
         Comprador = club_2,
         Pais_Comprador = country_2) %>%
  select(Jogador, Nome, Idade, Posicao, Liga, Temporada, 
         Janela, Valor, Time, Comprador, Pais_Comprador)

###WRANGLING SÉRIE B 2022
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_serieb_22 <- left_join(transf_serieb_22,jogadores_serieb_22)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_serieb_22 <- distinct(transf_serieb_22, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2022 SÉRIE B
#REDUÇÃO DE 477 PARA 66 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_22_serieb <- transf_serieb_22 %>%
  select(player_name, full_name, player_age, player_position, league,
         season, transfer_type, is_loan, transfer_fee, 
         window, team_name, club_2, league_2, country_2) %>%
  filter(is_loan == FALSE & transfer_fee >= 0 & transfer_type == "Departures") %>%
  rename(Jogador = player_name,
         Nome = full_name,
         Idade = player_age,
         Posicao = player_position,
         Liga = league,
         Temporada = season,
         Janela = window,
         Valor = transfer_fee,
         Time = team_name,
         Comprador = club_2,
         Pais_Comprador = country_2) %>%
  select(Jogador, Nome, Idade, Posicao, Liga, Temporada, 
         Janela, Valor, Time, Comprador, Pais_Comprador)

#BIND ROWS DAS TRANSFERÊNCIAS COM NOMES COMPLETOS - SÉRIES A E B - 351 OBSERVAÇÕES
brasil_22_consolidado <- bind_rows(brasil_22_seriea, brasil_22_serieb)

#AJUSTES DAS POSIÇÕES DOS JOGADORES E DA TEMPORADA, E TRADUÇÃO DAS JANELAS
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% "Attacking Midfield"] <- 'Meia-Atacante'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% "Centre-Forward"] <- 'Centroavante'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% c("Centre-Back", "defence")] <- 'Zagueiro'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% c("Right Winger", "Right Midfield")] <- 'Extremo-Direito'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% c("Left Midfield", "Left Winger")] <- 'Extremo-Esquerdo'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% "Left-Back"] <- 'Lateral-Esquerdo'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% c("Central Midfield", "midfield")] <- 'Meia'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% "Defensive Midfield"] <- 'Volante'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% "Right-Back"] <- 'Lateral-Direito'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% c("Second Striker", "attack")] <- 'Atacante'
brasil_22_consolidado$Posicao[brasil_22_consolidado$Posicao %in% "Goalkeeper"] <- 'Goleiro'
brasil_22_consolidado$Janela[brasil_22_consolidado$Janela %in% "Summer"] <- "Verão"
brasil_22_consolidado$Janela[brasil_22_consolidado$Janela %in% "Winter"] <- "Inverno"
brasil_22_consolidado$Temporada[brasil_22_consolidado$Temporada == "2021"] <- "2022"

#AJUSTES DOS NOMES DOS 40 TIMES
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Sociedade Esportiva Palmeiras"] <- 'Palmeiras'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "CR Flamengo"] <- 'Flamengo'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in%  "Sport Club Corinthians Paulista"] <- 'Corinthians'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Clube Atlético Mineiro"] <- 'Atlético-MG'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Sport Club Internacional"] <- 'Internacional'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Botafogo de Futebol e Regatas"] <- 'Botafogo'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Club Athletico Paranaense"] <- 'Athletico'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "São Paulo Futebol Clube"] <- 'São Paulo'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Santos FC"] <- 'Santos'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Red Bull Bragantino"] <- 'RB Bragantino'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Fluminense Football Club"] <- 'Fluminense'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Fortaleza Esporte Clube"] <- 'Fortaleza'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Avaí Futebol Clube (SC)"] <- 'Avaí'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Ceará Sporting Club"] <- 'Ceará'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Cuiabá Esporte Clube (MT)"] <- 'Cuiabá'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Coritiba Foot Ball Club"] <- 'Coritiba'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "América Futebol Clube (MG)"] <- 'América-MG'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Esporte Clube Juventude"] <- 'Juventude'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Goiás Esporte Clube"] <- 'Goiás'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Atlético Clube Goianiense"] <- 'Atlético-GO'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Grêmio Foot-Ball Porto Alegrense"] <- 'Grêmio'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Clube de Regatas Vasco da Gama"] <- 'Vasco'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Club de Regatas Vasco da Gama"] <- 'Vasco'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Cruzeiro Esporte Clube"] <- 'Cruzeiro'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Esporte Clube Bahia"] <- 'Bahia'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Associação Atlética Ponte Preta"] <- 'Ponte Preta'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Sport Club do Recife"] <- 'Sport'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Clube de Regatas Brasil (AL)"] <- 'CRB'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Clube Náutico Capibaribe"] <- 'Náutico'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Associação Chapecoense de Futebol"] <- 'Chapecoense'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Centro Sportivo Alagoano (AL)"] <- 'CSA'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Grêmio Novorizontino (SP)"] <- 'Novorizontino'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Guarani Futebol Clube (SP)"] <- 'Guarani'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Operário Ferroviário Esporte Clube (PR)"] <- 'Operário-PR'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Vila Nova Futebol Clube (GO)"] <- 'Vila Nova'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Londrina Esporte Clube (PR)"] <- 'Londrina'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Criciúma Esporte Clube"] <- 'Criciúma'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Ituano Futebol Clube (SP)"] <- 'Ituano'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Sampaio Corrêa Futebol Clube (MA)"] <- 'Sampaio Corrêa'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Tombense Futebol Clube (MG)"] <- 'Tombense'
brasil_22_consolidado$Time[brasil_22_consolidado$Time %in% "Brusque Futebol Clube (SC)"] <- 'Brusque'

#Inserção de dados do jogador Endrick
endrick <- data.frame("Endrick", "Endrick Felipe Moreira de Sousa", "16", "Extremo-Esquerdo",
                "Série A", "2022", "Inverno", 60000000, "Palmeiras", "Real Madrid", "Spain")
colnames(endrick) <- c("Jogador", "Nome", "Idade", "Posicao", "Liga", "Temporada",
                       "Janela", "Valor", "Time", "Comprador", "Pais_Comprador")
brasil_22_consolidado <- bind_rows(brasil_22_consolidado, endrick)

#Inserção de dados do jogador Wellington Rato
rato <- data.frame("Wellington Rato", "Wellington Soares da Silva", "30", "Extremo-Esquerdo",
                      "Série A", "2022", "Inverno", 891000, "Atlético-GO", "São Paulo", "Brazil")

colnames(rato) <- c("Jogador", "Nome", "Idade", "Posicao", "Liga", "Temporada",
                       "Janela", "Valor", "Time", "Comprador", "Pais_Comprador")

brasil_22_consolidado <- bind_rows(brasil_22_consolidado, rato)

#CORREÇÃO DE REGISTROS DO TRANSFERMARKT - EXCLUSÃO DE DUPLICATAS NA VARIÁVEL JOGADOR
#155 OBSERVAÇÕES
brasil_22_consolidado <- brasil_22_consolidado %>% 
  distinct(Jogador, .keep_all = TRUE) 

###DADOS DE DESEMPENHO - WYSCOUT - SÉRIES A E B
#IMPORTAR DADOS
wyscout_seriea_22 <- read_excel("wyscout_seriea_2022.xlsx")

wyscout_serieb_22 <- read_excel("wyscout_serieb_2022.xlsx")

#BIND ROWS SÉRIES A E B
#1555 OBSERVAÇÕES
wyscout_2022 <- bind_rows(wyscout_seriea_22, wyscout_serieb_22)

#WRANGLING DADOS DE DESEMPENHO (AJUSTE DE NOMES DE VARIÁVEIS)
wyscout_2022 <- wyscout_2022 %>% 
  rename(Jogador = player_name,
         Time = team_name,
         Jogos = `Matches played`,
         Minutos = `Minutes played`,
         Gols = Goals,
         Gols_Cabeca = `Head goals`,
         Gols_sem_Penalti = `Non-penalty goals`,
         xG_media = `xG per 90`,
         Conversao = `Goal conversion, %`,
         Acoes_Ataque_media = `Successful attacking actions per 90`,
         Toques_Area_media = `Touches in box per 90`,
         Conducoes_media = `Progressive runs per 90`,
         Faltas_Recebidas_media = `Fouls suffered per 90`,
         Assistencias = Assists,
         Passes_Chave_media = `Shot assists per 90`,
         Pre_Assistencia_media = `Second assists per 90`,
         Preparacao = `Third assists per 90`,
         Passes_Profundidade_media = `Deep completions per 90`,
         Distância_passes = `Average pass length, m`,
         Passes_Recebidos_media = `Received passes per 90`,
         Acoes_Defesa_media = `Successful defensive actions per 90`,
         Carrinhos_media = `Sliding tackles per 90`,
         Bloqueios_media = `Shots blocked per 90`,
         Interceptacoes_media = `Interceptions per 90`,
         Faltas_Cometidas_media = `Fouls per 90`,
         Amarelos = `Yellow cards`,
         Vermelhos = `Red cards`,
         Gols_Concedidos = `Conceded goals`,
         Gols_Concedidos_media = `Conceded goals per 90`,
         Gols_Evitados_media = `Prevented goals per 90`,
         Jogos_Invicto = `Clean sheets`,
         Percentual_Defesas = `Save rate, %`,
         Passes_Recebidos_Goleiro_media = `Back passes received as GK per 90`) %>%
  mutate(Minutos_media = Minutos / Jogos,
         Chutes_media = (`Shots per 90` * `Shots on target, %`) / 100,
         Cruzamentos_media = (`Crosses per 90` * `Accurate crosses, %`) / 100,
         Dribles_media = (`Dribbles per 90` * `Successful dribbles, %`) / 100,
         Passes_media = (`Passes per 90` * `Accurate passes, %`) / 100,
         Passes_Frente_media = (`Forward passes per 90` * 
                                  `Accurate forward passes, %`) / 100,
         Passes_Terco_Final_media = (`Passes to final third per 90` * 
                                       `Accurate passes to final third, %`) / 100,
         Passes_Area_media = (`Passes to penalty area per 90` * 
                                `Accurate passes to penalty area, %`) / 100,
         Passes_Progressivos_media = (`Progressive passes per 90` * 
                                        `Accurate progressive passes, %`) / 100,
         Duelos_Ataque_media = (`Offensive duels per 90` * 
                                  `Offensive duels won, %`) /  100,
         Duelos_Defesa_media = (`Defensive duels per 90` * 
                                  `Defensive duels won, %`) /  100,
         Duelos_Aereos_media = (`Aerial duels per 90` * 
                                  `Aerial duels won, %`) / 100) %>%
  select(Jogador, Time, Jogos, Minutos, Minutos_media, Gols,
         Gols_Cabeca, Gols_sem_Penalti, xG, xG_media, Conversao, Chutes_media,
         Acoes_Ataque_media, Toques_Area_media, Conducoes_media, Dribles_media, 
         Duelos_Ataque_media, Faltas_Recebidas_media, Assistencias, 
         Passes_Chave_media, Pre_Assistencia_media, Preparacao, Passes_media, 
         Passes_Recebidos_media, Distância_passes, Passes_Frente_media, 
         Passes_Profundidade_media, Passes_Progressivos_media, Passes_Terco_Final_media, 
         Passes_Area_media, Cruzamentos_media, Acoes_Defesa_media, Duelos_Defesa_media, 
         Duelos_Aereos_media, Carrinhos_media, Bloqueios_media, Interceptacoes_media,
         Faltas_Cometidas_media, Amarelos, Vermelhos, Gols_Concedidos,
         Gols_Concedidos_media, Gols_Evitados_media, Jogos_Invicto,
         Percentual_Defesas, Passes_Recebidos_Goleiro_media) %>%
  mutate_if(is.numeric, round, 2)

#FUSÃO DOS DOIS DATA FRAMES - DIVERSOS PROBLEMAS COM DIFERENTES GRAFIAS DE NOMES E/OU CLUBES TROCADOS
brasil_22_merge <- merge(brasil_22_consolidado, wyscout_2022, all = TRUE)

# AJUSTE MANUAL DE 106 GRAFIAS DE NOMES DE JOGADORES OU DE TIMES DE PROCEDÊNCIA NA BASE DO WYSCOUT
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Adriano' & wyscout_2022$Time %in% 'Cruzeiro', 'Adriano Firmino', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Arthur' & wyscout_2022$Time %in% 'Náutico', 'Cruzeiro', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Abner' & wyscout_2022$Time %in% 'Athletico Paranaense', 'Athletico', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Adriano' & wyscout_2022$Time %in% 'Santa Clara', 'Cruzeiro', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Airton' & wyscout_2022$Time %in% 'América Mineiro', 'América-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Alan Ruschel' & wyscout_2022$Time %in% 'Londrina', 'Cruzeiro', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Anderson Leite' & wyscout_2022$Time %in% 'Juventude', 'Chapecoense', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Andrey Santos' & wyscout_2022$Time %in% 'Vasco da Gama', 'Vasco', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'A. Landázuri' & wyscout_2022$Time %in% 'Fortaleza', 'Anthony Landázuri', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Aylon' & wyscout_2022$Time %in% 'Ituano', 'CSA', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Bruninho' & wyscout_2022$Time %in% 'CRB', 'Ituano', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'B. Angulo', 'Bryan Angulo', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Calyson' & wyscout_2022$Time %in% 'ABC', 'CRB', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Calyson', 'Calyson Rosa', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Carlão' & wyscout_2022$Time %in% 'Ituano', 'Guarani', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'C. Sánchez', 'Carlos Sánchez', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Ciel' & wyscout_2022$Time %in% 'Tombense', 'Sampaio Corrêa', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Danielzinho' & wyscout_2022$Time %in% 'Grêmio Novorizontino', 'Daniel Jesus', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Daniel Jesus' & wyscout_2022$Time %in% 'Grêmio Novorizontino', 'Novorizontino', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Danilo Barcelos' & wyscout_2022$Time %in% 'Goiás', 'Fluminense', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Derlan' & wyscout_2022$Time %in% 'Guarani', 'Chapecoense', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Deyverson' & wyscout_2022$Time %in% 'Cuiabá', 'Palmeiras', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Diego Torres' & wyscout_2022$Time %in% 'Grêmio Novorizontino', 'CRB', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Ednei' & wyscout_2022$Time %in% 'Tombense', 'Ponte Preta', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Edson Fernando' & wyscout_2022$Time %in% 'Atlético GO', 'Bahia', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Eduardo' & wyscout_2022$Time %in% 'Sport Recife', 'América-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Élton' & wyscout_2022$Time %in% 'CSA', 'Cuiabá', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Elvis' & wyscout_2022$Time %in% 'Ponte Preta', 'Élvis', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Élvis' & wyscout_2022$Time %in% 'Ponte Preta', 'Goiás', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Emerson Santos' & wyscout_2022$Time %in% 'Bahia', 'Émerson Santos', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'E. Rigoni', 'São Paulo', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'E. Rigoni', 'Emiliano Rigoni', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'E. Velázquez', 'Santos', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'E. Velázquez', 'Emiliano Velázquez', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Ernando', 'Vasco', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Ewerton', 'Ewerton Páscoa', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Ewerton Páscoa', 'CRB', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'F. Henrique', 'Fernando Henrique', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Gabriel Sara', 'São Paulo', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Gabriel Veron', 'Palmeiras', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Geovane' & wyscout_2022$Time %in% 'Cruzeiro', 'Geovane Jesus', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Guga', 'Atlético-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Guilherme Castilho', 'Atlético-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Gustavo Lopes', 'Operário-PR', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Gustavo Marques', 'América-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Hugo Moura', 'Flamengo', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'H. Rodallega', 'Hugo Rodallega', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Igor Paixão', 'Coritiba', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Jair', 'Atlético-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Jefferson Paulino', 'Brusque', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Jô', 'Corinthians', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'João Victor' & wyscout_2022$Time %in% 'Benfica', 'Corinthians', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Joílson', 'Chapecoense', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Jonas Toró', 'São Paulo', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Jonathan' & wyscout_2022$Time %in% 'CSA', 'Bahia', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Kadu', 'Chapecoense', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Kaiky', 'Santos', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Keno', 'Atlético-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Lázaro', 'Flamengo', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Léo Baptistão', 'Santos', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Luan Polli', 'Sport', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Lucão' & wyscout_2022$Time %in% 'Red Bull Bragantino', 'Vasco', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Lucas Fasson', 'Athletico', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Lucas Frigeri', 'CSA', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Luccas Claro', 'Fluminense', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Luciano Castán', 'Luciano Castan', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Luiz Henrique' & wyscout_2022$Time %in% 'Real Betis', 'Fluminense', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Marcondes', 'Londrina', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Marlon' & wyscout_2022$Time %in% 'Ankaragücü', 'Fluminense', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Marquinhos Gabriel', 'Vasco', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Matheus Frizo', 'Grêmio', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Matheus Mancini', 'Ituano', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Matheus Ribeiro', 'Chapecoense', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'M. Isla', 'Mauricio Isla', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Mauricio Isla', 'Flamengo', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'I. Fernández', 'Nacho Fernández', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Nacho Fernández', 'Atlético-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Nadson', 'CSA', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Nadson', 'Nádson', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Natanael' & wyscout_2022$Time %in% 'Avaí', 'Internacional', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Nathan' & wyscout_2022$Time %in% 'Santos', 'Vasco', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Neilton', 'Neílton', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Nino Paraíba', 'Bahia', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Norberto Neto', 'Norberto', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Norberto' & wyscout_2022$Time %in% 'Ponte Preta', 'Cruzeiro', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Patric', 'América-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Rafinha' & wyscout_2022$Time %in% 'São Paulo', 'Grêmio', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'R. Cáceres', 'Raúl Cáceres', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Raúl Cáceres', 'América-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Renan Bressan', 'CRB', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Ricardo Bueno', 'Operário-PR', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Ricardo Goulart', 'Santos', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Rodrigo Andrade', 'Sampaio Corrêa', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Rodriguinho' & wyscout_2022$Time %in% 'Cuiabá', 'Bahia', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Saimon', 'Vila Nova', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Sávio' & wyscout_2022$Time %in% 'PSV', 'Atlético-MG', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Silas', 'CSA', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Silvinho', 'CSA', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Vitinho' & wyscout_2022$Time %in% 'Al Ettifaq', 'Flamengo', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Weverson', 'RB Bragantino', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Willian' & wyscout_2022$Time %in% 'Fulham', 'Corinthians', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Willian Arão', 'Flamengo', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Yuri' & wyscout_2022$Time %in% 'Vasco da Gama', 'Vasco', wyscout_2022$Time)
wyscout_2022$Jogador <- ifelse(wyscout_2022$Jogador %in% 'Yuri' & wyscout_2022$Time %in% 'Vasco', 'Yuri Lara', wyscout_2022$Jogador)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Zé Ricardo' & wyscout_2022$Time %in% 'Tombense', 'Londrina', wyscout_2022$Time)
wyscout_2022$Time <- ifelse(wyscout_2022$Jogador %in% 'Wellington Rato', 'Atlético-GO', wyscout_2022$Time)

#REPETE A FUSÃO DOS DOIS DATA FRAMES APÓS AS CORREÇÕES
brasil_22_merge <- merge(brasil_22_consolidado, wyscout_2022, all = TRUE)

###PROSPECÇÃO DE DADOS DE DESEMPENHO DA TEMPORADA 2022
###PARA JOGADORES TRANSFERIDOS EM 2022 SEM DADOS DE DESEMPENHO EM 2022
#FILTRO DA BASE DE DADOS BRASIL_22_MERGE PELA AUSÊNCIA DE NA's NA VARIÁVEL JOGOS
brasil_22_sobras <- brasil_22_merge %>% 
  filter(!is.na(Nome) & is.na(Jogos)) %>% 
  select(1:11)

#EXCLUSÃO DE NA's NA VARIÁVEL NOME E JOGOS
#EXCLUSÃO DE DUPLICATAS NA VARIÁVEIS NOME E JOGOS
#115 OBSERVAÇÕES
brasil_22_merge <- brasil_22_merge %>% 
  filter(!is.na(Nome) & !is.na(Jogos)) %>% 
  distinct(Nome, .keep_all = TRUE) %>% 
  distinct(Jogador, .keep_all = TRUE) 

#MERGE DAS TRANSFERÊNCIAS QUE SOBRARAM DE 2022 COM OS DADOS DE DESEMPENHO DE 2021
# (TÊM DADOS DE TRANSFERÊNCIA MAS NÃO TÊM DE DESEMPENHO)
brasil_22_sobras <- merge(brasil_22_sobras, wyscout_2021,  all = TRUE)

# AJUSTE MANUAL DE 6 GRAFIAS DE NOMES DE JOGADORES OU DE TIMES DE PROCEDÊNCIA NA BASE DO WYSCOUT
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Anderson Paixão', 'Athletico', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Á. Henríquez', 'Ángelo Henríquez', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Gabriel Boschilia', 'Boschilia', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Djalma Silva', 'Bahia', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Michel' & wyscout_2021$Time %in% 'Vasco', 'Grêmio', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'O. Berrío', 'Orlando Berrío', wyscout_2021$Jogador)

#REPETE A FUSÃO DOS DOIS DATA FRAMES APÓS AS CORREÇÕES
brasil_22_sobras <- merge(brasil_22_sobras, wyscout_2021,  all = TRUE)

#EXCLUSÃO DE OBSERVAÇÕES COM NA'S EM JOGOS & VALOR = 0
#SUBSTITUIÇÃO DE NA'S POR 0 PARA JOGADORES COM VALOR > 0
#20 OBSERVAÇÕES
brasil_22_sobras <- brasil_22_sobras %>% 
  filter(!is.na(Nome)) %>% 
  distinct(Nome, .keep_all = TRUE) %>% 
  distinct(Jogador, .keep_all = TRUE) %>%
  mutate(Jogos = ifelse(Valor > 0 & is.na(Jogos), 0, Jogos)) %>% 
  filter(!is.na(Jogos)) %>%
  mutate(across(12:55, replace_na, 0))

#BIND ROWS DOS BANCOS DE TRANSFERÊNCIAS 2020/DESEMPENHO 2020 E TRANSFERÊNCIAS 2020/DESEMPENHO 2019
brasil_transferencias_2022 <- bind_rows(brasil_22_merge, brasil_22_sobras)

#CONSOLIDAÇÃO DO DATA FRAME
#135 OBSERVAÇÕES SÉRIES A E B 2021
#32 DAS 135 COM VALOR DE TRANSFERÊNCIA
brasil_transferencias_2022 <- as.data.frame(brasil_transferencias_2022)

#EXPORTAÇÃO PARA ARQUIVO XSML
write_xlsx(brasil_transferencias_2022, "brasil_transferencias_2022.xlsx")
