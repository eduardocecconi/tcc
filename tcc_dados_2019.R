library(tidyverse)
library(readxl)
library(writexl)
library(stats)

###DADOS DE TRANSFERÊNCIAS - 2019 - SÉRIES A E B

#CARREGAR LISTAS DE TRANSFERÊNCIAS
transf_seriea_19 <- read_excel("tm_transferencias_seriea_2019.xlsx")

transf_serieb_19 <- read_excel("tm_transferencias_serieb_2019.xlsx")

#CARREGAR BIOGRAFIA DOS JOGADORES
jogadores_seriea_19 <- read_excel("tm_jogadores_seriea_2019.xlsx")

jogadores_serieb_19 <- read_excel("tm_jogadores_serieb_2019.xlsx")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA NAME_IN_HOME_COUNTRY
jogadores_seriea_19$full_name <- ifelse(is.na(jogadores_seriea_19$full_name), 
                                        jogadores_seriea_19$name_in_home_country, 
                                        jogadores_seriea_19$full_name)

jogadores_serieb_19$full_name <- ifelse(is.na(jogadores_serieb_19$full_name), 
                                        jogadores_serieb_19$name_in_home_country, 
                                        jogadores_serieb_19$full_name)

#SELECIONAR O NOME DO JOGADOR NA COLUNA PLAYER_NAME
jogadores_seriea_19$player_name <- word(jogadores_seriea_19$player_name, -2, -1)

jogadores_serieb_19$player_name <- word(jogadores_serieb_19$player_name, -2, -1)

#EXCLUIR ESPAÇOS NO INÍCIO DO NOME DOS JOGADORES
jogadores_seriea_19$player_name <- str_trim(jogadores_seriea_19$player_name, side = "both")

jogadores_serieb_19$player_name <- str_trim(jogadores_serieb_19$player_name, side = "both")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA PLAYER_NAME
jogadores_seriea_19 <- jogadores_seriea_19 %>% 
  mutate(full_name = ifelse(is.na(jogadores_seriea_19$full_name), 
                            jogadores_seriea_19$player_name, jogadores_seriea_19$full_name)) %>%
  select(player_name, full_name)

jogadores_serieb_19 <- jogadores_serieb_19 %>% 
  mutate(full_name = ifelse(is.na(jogadores_serieb_19$full_name), 
                            jogadores_serieb_19$player_name, jogadores_serieb_19$full_name)) %>%
  select(player_name, full_name)

#SELECIONAR A COMPETIÇÃO NA COLUNA LEAGUE DO BANCO DE TRANSFERÊNCIAS
transf_seriea_19$league <- word(transf_seriea_19$league, -2, -1)

transf_serieb_19$league <- word(transf_serieb_19$league, -2, -1)

###WRANGLING SÉRIE A 2019
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_seriea_19 <- left_join(transf_seriea_19,jogadores_seriea_19)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_seriea_19 <- distinct(transf_seriea_19, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2019 SÉRIE A
#REDUÇÃO DE 696 PARA 82 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_19_seriea <- transf_seriea_19 %>%
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

###WRANGLING SÉRIE B 2019
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_serieb_19 <- left_join(transf_serieb_19,jogadores_serieb_19)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_serieb_19 <- distinct(transf_serieb_19, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2019 SÉRIE B
#REDUÇÃO DE 932 PARA 166 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_19_serieb <- transf_serieb_19 %>%
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

#BIND ROWS DAS TRANSFERÊNCIAS COM NOMES COMPLETOS - SÉRIES A E B - 248 OBSERVAÇÕES
brasil_19_consolidado <- bind_rows(brasil_19_seriea, brasil_19_serieb)

#AJUSTES DAS POSIÇÕES DOS JOGADORES E TRADUÇÃO DAS JANELAS
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% "Attacking Midfield"] <- 'Meia-Atacante'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% "Centre-Forward"] <- 'Centroavante'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% c("Centre-Back", "defence")] <- 'Zagueiro'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% c("Right Winger", "Right Midfield")] <- 'Extremo-Direito'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% c("Left Midfield", "Left Winger")] <- 'Extremo-Esquerdo'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% "Left-Back"] <- 'Lateral-Esquerdo'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% "Central Midfield"] <- 'Meia'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% "Defensive Midfield"] <- 'Volante'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% "Right-Back"] <- 'Lateral-Direito'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% "Second Striker"] <- 'Atacante'
brasil_19_consolidado$Posicao[brasil_19_consolidado$Posicao %in% "Goalkeeper"] <- 'Goleiro'
brasil_19_consolidado$Janela[brasil_19_consolidado$Janela %in% "Summer"] <- "Verão"
brasil_19_consolidado$Janela[brasil_19_consolidado$Janela %in% "Winter"] <- "Inverno"

#AJUSTES DOS NOMES DOS 37 TIMES
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Sport Club Corinthians Paulista"] <- 'Corinthians'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Clube de Regatas do Flamengo"] <- 'Flamengo'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Sociedade Esportiva Palmeiras"] <- 'Palmeiras'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Clube Atlético Mineiro"] <- 'Atlético-MG'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Sport Club Internacional"] <- 'Internacional'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Grêmio Foot-Ball Porto Alegrense"] <- 'Grêmio'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "São Paulo Futebol Clube"] <- 'São Paulo'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Santos FC"] <- 'Santos'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Esporte Clube Bahia"] <- 'Bahia'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Club de Regatas Vasco da Gama"] <- 'Vasco'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Coritiba Foot Ball Club"] <- 'Coritiba'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Club Athletico Paranaense"] <- 'Athletico'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Botafogo de Futebol e Regatas"] <- 'Botafogo'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Goiás Esporte Clube"] <- 'Goiás'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Sport Club do Recife"] <- 'Sport'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Ceará Sporting Club"] <- 'Ceará'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Atlético Clube Goianiense"] <- 'Atlético-GO'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Cruzeiro Esporte Clube"] <- 'Cruzeiro'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Avaí Futebol Clube (SC)"] <- 'Avaí'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Centro Sportivo Alagoano (AL)"] <- 'CSA'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Clube de Regatas Brasil (AL)"] <- 'CRB'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Esporte Clube Vitória"] <- 'Vitória'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Associação Chapecoense de Futebol"] <- 'Chapecoense'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Esporte Clube Juventude"] <- 'Juventude'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Associação Atlética Ponte Preta"] <- 'Ponte Preta'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Botafogo Futebol Clube (SP)"] <- 'Botafogo-SP'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Cuiabá Esporte Clube (MT)"] <- 'Cuiabá'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Figueirense Futebol Clube"] <- 'Figueirense'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Guarani Futebol Clube (SP)"] <- 'Guarani'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Clube Náutico Capibaribe"] <- 'Náutico'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Oeste Futebol Clube (SP)"] <- 'Oeste'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Paraná Clube"] <- 'Paraná'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Operário Ferroviário Esporte Clube (PR)"] <- 'Operário-PR'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Grêmio Esportivo Brasil (RS)"] <- 'Brasil-RS'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Associação Desportiva Confiança (SE)"] <- 'Confiança'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Sampaio Corrêa Futebol Clube (MA)"] <- 'Sampaio Corrêa'
brasil_19_consolidado$Time[brasil_19_consolidado$Time %in% "Fluminense Football Club"] <- 'Fluminense'

#CORREÇÃO DE REGISTROS DO TRANSFERMARKT - EXCLUSÃO DE DUPLICATAS NA VARIÁVEL JOGADOR
#230 OBSERVAÇÕES
brasil_19_consolidado <- brasil_19_consolidado %>% 
  distinct(Jogador, .keep_all = TRUE) 

#ALTERAÇÃO PONTUAL DE UM REGISTRO NA VARIÁVEL TIME
brasil_19_consolidado$Time <- ifelse(brasil_19_consolidado$Jogador %in% 'Édson', 'Ponte Preta', brasil_19_consolidado$Time)

###DADOS DE DESEMPENHO - WYSCOUT - SÉRIES A E B
#IMPORTAR DADOS
wyscout_seriea_19 <- read_excel("wyscout_seriea_2019.xlsx")

wyscout_serieb_19 <- read_excel("wyscout_serieb_2019.xlsx")

#BIND ROWS SÉRIES A E B
#1497 OBSERVAÇÕES
wyscout_2019 <- bind_rows(wyscout_seriea_19, wyscout_serieb_19)

#WRANGLING DADOS DE DESEMPENHO (AJUSTE DE NOMES DE VARIÁVEIS)
wyscout_2019 <- wyscout_2019 %>% 
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
brasil_19_merge <- merge(brasil_19_consolidado, wyscout_2019, all = TRUE)

# AJUSTE MANUAL DE 44 GRAFIAS DE NOMES DE JOGADORES OU DE TIMES DE PROCEDÊNCIA NA BASE DO WYSCOUT
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Alex Silva', 'Atlético-MG', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Alecsandro' & wyscout_2019$Time %in% 'CSA', 'Alexsandro', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Augusto' & wyscout_2019$Time %in% 'Chapecoense', 'Augusto César', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'B. Ramires', 'Bruno Ramires', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Caio Rangel', 'São Bento', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Danilo Báia', 'Danilo Baia', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Dênis', 'Denis', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Edson Borges', 'Cuiabá', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Edson' & wyscout_2019$Time %in% 'Ponte Preta', 'Édson', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Eduardo Kunde', 'Eduardo Kau', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Erick' & wyscout_2019$Time %in% 'Botafogo-SP', 'Erick Luis', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Fabricio Bruno', 'Fabrício Bruno', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Fernando Henrique', 'Ceará', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Gegé', 'Gegê', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Gerson' & wyscout_2019$Time %in% 'CSA', 'Gerson Júnior', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'G. Cuéllar', 'Gustavo Cuéllar', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Jonathan' & wyscout_2019$Time %in% 'Botafogo', 'Jonathan Silva', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Jonata Escobar', 'Escobar', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'J. Mosquera', 'Jonny Mosquera', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Kayke' & wyscout_2019$Time %in% 'Goiás', 'Kayke Rodrigues', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Willian Klaus', 'Klaus', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'L. Valencia', 'Leonardo Valencia', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'L. Romero', 'Lucas Romero', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Luis Henrique' & wyscout_2019$Time %in% 'Botafogo', 'Pachu', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'M. Trauco', 'Miguel Trauco', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Morato', 'São Paulo', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Murilo Henrique', 'Atlético-GO', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Murilo Henrique' & wyscout_2019$Time %in% 'Atlético-GO', 'Murilo', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Nenê', 'São Paulo', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'N. López', 'Nicolás López', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Norberto Neto', 'Norberto', wyscout_2019$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Orinho', 'Santos', wyscout_2018$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Pará' & wyscout_2019$Time %in% 'Santos', 'Flamengo', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Pedro Bambú', 'Pedro Bambu', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Raniel', 'Cruzeiro', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'R. Cáceres', 'Raúl Cáceres', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Ricardo Bueno', 'Ceará', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Rodrigo Viana', 'Botafogo-SP', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Thiago Carleto', 'Ceará', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Willian Farias', 'São Paulo', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Willie Barbosa', 'Willie', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Y. Chará', 'Yimmi Chará', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Zé Antonio', 'Zé Antônio', wyscout_2019$Jogador)

#REPETE A FUSÃO DOS DOIS DATA FRAMES APÓS AS CORREÇÕES
brasil_19_merge <- merge(brasil_19_consolidado, wyscout_2019, all = TRUE)

###SELEÇÃO DE JOGADORES TRANSFERIDOS EM 2019 SEM DADOS DE DESEMPENHO EM 2019
#FILTRO PELA AUSÊNCIA DE NA's NA VARIÁVEL NOME E PRESENÇA DE NA'S NA VARIÁVEL JOGOS
#67 OBSERVAÇÕES
brasil_19_sobras <- brasil_19_merge %>% 
  filter(!is.na(Nome) & is.na(Jogos)) %>% 
  select(1:11)

#EXCLUSÃO DE NA's NA VARIÁVEL NOME E JOGOS
#EXCLUSÃO DE DUPLICATAS NA VARIÁVEIS NOME E JOGOS
#162 OBSERVAÇÕES
brasil_19_merge <- brasil_19_merge %>% 
  filter(!is.na(Nome) & !is.na(Jogos)) %>% 
  distinct(Nome, .keep_all = TRUE) %>% 
  distinct(Jogador, .keep_all = TRUE) 

#MERGE DAS TRANSFERÊNCIAS QUE SOBRARAM DE 2019 COM OS DADOS DE DESEMPENHO DE 2018
# (TÊM DADOS DE TRANSFERÊNCIA MAS NÃO TÊM DE DESEMPENHO)
brasil_19_sobras <- merge(brasil_19_sobras, wyscout_2018,  all = TRUE)

# AJUSTE MANUAL DE 14 GRAFIAS DE NOMES DE JOGADORES OU DE TIMES DE PROCEDÊNCIA NA BASE DO WYSCOUT
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Á. Romero', 'Ángel Romero', wyscout_2018$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Caio Rangel', 'Paraná', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Danilo Pires', 'Náutico', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Diego Ivo', 'Juventude', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Lucas Taylor', 'Palmeiras', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Luiz Henrique', 'Bahia', wyscout_2018$Time)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Marcinho' & wyscout_2018$Time %in% "Oeste", 'Márcio Victor', wyscout_2018$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Matheus Lopes', 'Paraná', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Pingo', 'Confiança', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Renan Oliveira', 'Botafogo-SP', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Thomaz', 'São Paulo', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Tito', 'Confiança', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Wallace Pernambucano', 'Náutico', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Xandão', 'Bahia', wyscout_2018$Time)

#REPETE A FUSÃO DOS DOIS DATA FRAMES APÓS AS CORREÇÕES
brasil_19_sobras <- merge(brasil_19_sobras, wyscout_2018,  all = TRUE)

#EXCLUSÃO DE OBSERVAÇÕES COM NA'S EM JOGOS & VALOR = 0
#SUBSTITUIÇÃO DE NA'S POR 0 PARA JOGADORES COM VALOR > 0
#23 OBSERVAÇÕES
brasil_19_sobras <- brasil_19_sobras %>% 
  filter(!is.na(Nome)) %>% 
  distinct(Nome, .keep_all = TRUE) %>% 
  distinct(Jogador, .keep_all = TRUE) %>%
  mutate(Jogos = ifelse(Valor > 0 & is.na(Jogos), 0, Jogos)) %>% 
  filter(!is.na(Jogos)) %>%
  mutate(across(12:55, replace_na, 0))

#BIND ROWS DOS BANCOS DE TRANSFERÊNCIAS 2019/DESEMPENHO 2019 E TRANSFERÊNCIAS 2019/DESEMPENHO 2018
brasil_transferencias_2019 <- bind_rows(brasil_19_merge, brasil_19_sobras)

#CONSOLIDAÇÃO DO DATA FRAME
#185 OBSERVAÇÕES SÉRIES A E B 2019
#38 DAS 185 COM VALOR DE TRANSFERÊNCIA > 0
brasil_transferencias_2019 <- as.data.frame(brasil_transferencias_2019)

#EXPORTAÇÃO PARA ARQUIVO XSML
write_xlsx(brasil_transferencias_2019, "brasil_transferencias_2019.xlsx")
