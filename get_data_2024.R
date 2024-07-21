library(tidyverse)

# Proposições por ano de apresentação -------------------------------------
url <- str_glue('https://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-{1989:2023}.csv')
# Cria diretório
dir.create('data/proposições', showWarnings = FALSE)

# Baixa cada uma das proposições
walk(url, 
    
    ~{
      destfile = .x %>% str_extract("proposicoes-\\d{4}")
      download.file(.x, destfile = str_glue('data/proposições/{destfile}.csv'))
    }, .progress = TRUE
    )

# Lê todas as proposições
proposicoes <- read_delim(
  list.files('data/proposições', full.names = TRUE),
  delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Classificação temática das proposições ----------------------------------
# url proposições tematicas
url <- str_glue('https://dadosabertos.camara.leg.br/arquivos/proposicoesTemas/csv/proposicoesTemas-{1989:2023}.csv')

# Cria diretório
dir.create('data/proposições_tematicas', showWarnings = FALSE)

# Baixa cada uma das proposições
walk(url, 
     
     ~{
       destfile = .x %>% str_extract("proposicoesTemas-\\d{4}")
       download.file(.x, destfile = str_glue('data/proposições_tematicas/{destfile}.csv'))
     }, .progress = TRUE
)
proposicoes_tematicas <- read_delim(
  list.files('data/proposições_tematicas', full.names = TRUE),
  delim = ";", escape_double = FALSE, trim_ws = TRUE
)

# autores_proposições -----------------------------------------------------

url <- str_glue('https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-{1989:2023}.csv')

# Cria Diretório
dir.create('data/proposicoes_autores', showWarnings = FALSE)

walk(url, 
     
     ~{
       destfile = .x %>% str_extract("proposicoesAutores-\\d{4}")
       download.file(.x, destfile = str_glue('data/proposicoes_autores/{destfile}.csv'))
     }, .progress = TRUE
)

autores_proposicoes <- read_delim(
  list.files('data/proposicoes_autores', full.names = TRUE),
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) %>% 
  filter(
    tipoAutor == "Deputado(a)"
  ) %>% 
  select(
    idProposicao, nomeAutor, siglaPartidoAutor, siglaUFAutor,
    ordemAssinatura, proponente
  )
  glimpse()

  autores_proposicoes %>% 
    write_rds('data/autores_proposicoes.rds')
# Enriquecendo os dados

proposicoes_classificadas <- proposicoes_tematicas %>% 
  filter(tema == "Direito Penal e Processual Penal") %>% 
  left_join(
    proposicoes,
    join_by(uriProposicao == uri,
            numero == numero, 
            ano == ano,
            siglaTipo == siglaTipo)
  ) 

# proposições autoradas apenas por deputados
proposicoes_classificadas <- inner_join(proposicoes_classificadas, autores_proposicoes %>%
             select(idProposicao) %>% 
             unique(), 
           join_by(id == idProposicao))


write_rds(proposicoes_classificadas, "data/proposicoes_classificadas.rds")
# Baixando PL inteira -----------------------------------------------------


link_proposicao <- proposicoes_classificadas %>% 
  drop_na(urlInteiroTeor) %>% 
  pull(urlInteiroTeor)

dir.create('data/pdf_pl')  

walk(
  link_proposicao,
  ~{
    nome_arquivo <- basename(.x) %>% 
      str_extract("\\d+")
    
    download.file(.x, destfile = str_glue('data/pdf_pl/{nome_arquivo}.pdf'),
                  mode = "wb")
  }
)

# Dados Ocorrências criminais -------------------------------------------

fs::dir_create("data/indicadores_criminais")

url <- stringr::str_glue(
  'https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-{2015:2024}.xlsx/@@download/file'
)


walk(
  url,
  possibly(~{
    nome_arquivo <- .x %>% 
      str_extract('bancovde-\\d{4}')
    
    download.file(.x,
                  destfile = str_glue('data/indicadores_criminais/{nome_arquivo}.xlsx'),
                  mode = "wb")
    
  })
)

# importando dados

arquivos <- fs::dir_ls("data/indicadores_criminais")

indicadores_criminais <- purrr::map_dfr(arquivos, possibly(~{
  readxl::read_xlsx(.x)
}))

