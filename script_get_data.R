### Precisa consertar os labels das variáveis


library(tidyverse)

pega_pl<- function(ano = 1989){ 

url <- paste0(
  'https://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoes?sigla=PL&numero=&ano=',
  ano,
  '&datApresentacaoIni=&datApresentacaoFim=&parteNomeAutor=&idTipoAutor=&siglaPartidoAutor=&siglaUFAutor=&generoAutor=&codEstado=&codOrgaoEstado=&emTramitacao='
  
  
)
dados <- httr::GET(url) %>% 
  httr::content()

dados_final <- dados %>% 
  xml2::as_list() %>% 
  enframe() %>% 
  unnest_longer(value) %>% 
  select(value) %>% 
  unnest_wider(value) 
}

dados <- furrr::future_map_dfr(
  1989:2022,
  ~pega_pl(.x)
)


dados %>% 
  mutate(
    across(c(id, nome, numero, ano, datApresentacao,
             txtEmenta, txtExplicacaoEmenta, indGenero, 
             qtdOrgaosComEstado
             ), unlist)
  ) %>% 
  write_rds("dados_pl.rds")

