### Busca de Termos
library(tidyverse)
dados <-  readr::read_rds("data/dados_pl.rds") %>% 
  janitor::clean_names()


proposicoes_classificados <- readr::read_rds("data/proposicoes_classificadas.rds") %>% 
  janitor::clean_names()
# Termos

# Versão 1 com booleano de alternancia
termos <- c('(Aumento.*Pena)',
            '(Criminalidade.*Violenta)',
            '(Crime.*Hediondo)',
            '(Crimes.*Violentos)',
            '(Criminosos.*Reincidentes)',
            '(Impunidade.*Criminal)',
            '(Majoração.*Pena)',
            '(Medidas.*Punitivas)',
            '(Penalidades.*Aumentadas)',
            '(Penalização.*Rigorosa)',
            '(Penas.*Duras)',
            '(Penas.*Longas)',
            '(Progressão.*Regime)',
            '(Redução.*Maioridade) Penal',
            '(Reincidência.*Criminal)',
            'Rigor',
            '(Sanções.*Penais)')

# Versão 2 expandido em 3 dimensões
#1. sinônimos e variações dos termos 
#2. + expressões recrudescimento penal 
#3. + termos intenção de endurecer o sistema penal
# Buscar termos com regex para deixar o dicionário mais robusto
#\\bCrim\\w buscará qualquer termo que contenha a palavra crim
#, como crime, criminoso, criminal, etc.


termos <- c(
  #Inclusão sinônimos e variações:
  '(Aumento.*\\bPen\\w*)',
  '(Criminalidade.*\\bViol\\w*)',
  '(\\bCrime.*\\bHedion\\w*)',
  '(\\bCrim\\w*.*\\bViol\\w*)',
  '(\\bCriminos\\w*.*\\bReincid\\w*)',
  '(\\bImpunidad\\w*.*\\bCrim\\w*)',
  '(Majoração.*\\bPen\\w*)',
  '(Medidas.*\\bPunitiv\\w*)',
  '(Penalidades.*\\bAument\\w*)',
  '(Penalização.*\\bRigoros\\w*)',
  '(\\bPen\\w*.*\\bDur\\w*)',
  '(\\bPen\\w*.*\\bLong\\w*)',
  '(Progressão.*\\bRegim\\w*)',
  '(Redução.*Maioridade.*\\bPen\\w*)',
  '(\\bReincid\\w*.*\\bCrim\\w*)',
  '(\\bRigor|\\bRigoros\\w*)',
  '(Sanções.*\\bPen\\w*)',
  '(Regime.*\\bFechad\\w*|Regime.*\\bPrisional)',
  # Inclusão recrudescimento penal:
  '(Aumento.*\\bReclus\\w*)',
  '(\\bPen\\w*.*\\bReclusiv\\w*)',
  '(Endurecimento.*\\bPen\\w*)',
  '(Punição.*\\bSever\\w*)',
  '(Punições.*\\bRigoros\\w*)',
  '(Controle.*\\bPen\\w*)',
  '(Medidas.*\\bRepressor\\w*)',
  '(Lei.*\\bAnti.*\\bCrim\\w*)',
  '(Endurecimento.*\\bLei\\w*)',
  '(Política.*\\bCriminal)',
  # Inclusão termos intenção de endurecer o sistema penal
  '(\\bCriminaliza\\w*)',
  '(Endurecimento.*\\bRegim\\w*)',
  '(Combate.*\\bCrim\\w*)',
  '(Intensificação.*\\bPen\\w*)',
  '(Agravar.*\\bPen\\w*)') 


termos <- termos %>% 
  stringr::str_to_lower() %>% 
  stringr::str_flatten(collapse = '|')


# Classificação busca simples
dados_classificados <- dados %>% 
  select(
    id, nome, ano, txt_ementa, txt_explicacao_ementa
  ) %>% 
  mutate(
    txt_ementa = stringr::str_to_lower(txt_ementa),
    txt_explicacao_ementa = stringr::str_to_lower(txt_explicacao_ementa),
    # motor = txt_ementa+ txt_explicacao_ementa
    termo_motor = paste0(txt_ementa, txt_explicacao_ementa, sep = " "),
    termo_recrudescimento = stringr::str_detect(termo_motor, termos)
  ) 



proposicoes_classificados <- proposicoes_classificados %>% 
 mutate(
   across(c(ementa, ementa_detalhada), str_to_lower),
   termo_motor = paste0(ementa, ementa_detalhada, sep = " "),
   termo_recrudescimento = str_detect(termo_motor, termos)
 )



# Verificando a quantidade de termos classificados
dados_classificados %>% 
  janitor::tabyl(
    termo_recrudescimento
  )

proposicoes_classificados %>% 
  janitor::tabyl(
    termo_recrudescimento
  )



# Validação do Sentimento com Vader ---------------------------------------
library(sentimentr)

sentencas_motor <- get_sentences(proposicoes_classificados$termo_motor)


  sentimentr::as_key()
obj_sentimentos <- sentiment(
  sentencas_motor,
  polarity_dt = lexiconPT::oplexicon_v3.0 %>% 
    select(term, polarity) %>% 
    sentimentr::as_key(),
  algorithm = "vader"
)

# Média do sentimento para PL

proposicoes_sentimentos <- proposicoes_classificados %>% 
  mutate(
    sentimento = obj_sentimentos %>% 
      tibble() %>% 
      summarise(
        .by = element_id,
        sentimento = mean(sentiment, na.rm = TRUE)
      ) %>% 
      pull(sentimento)
    ) 

proposicoes_sentimentos %>% 
  lm(
    sentimento ~ termo_recrudescimento,
    data = .
  ) %>%
  broom::tidy()

proposicoes_sentimentos %>% 
write_rds("propo_sentimentos.rds")
