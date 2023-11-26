
# Separar componentes do nome ---------------------------------------------

extrair_familia <- function(s) {
  str_split_i(s, ',[ ]*', 1)
}

extrair_titulo <- function(s) {
  s <- str_split_i(s, ',[ ]*', 2)
  str_split_i(s, stringr::fixed('.'), 1)
}

extrair_nome <- function(s) {
  s <- str_split_i(s, ',[ ]*', 2)
  s <- str_split_i(s, stringr::fixed('.'), 2)
  str_trim(s)
}


# Receita para logo depois da leitura -------------------------------------

receita_pos_leitura <- function(df) {
  
  # Passos comuns a todos os dfs
  rv <- recipe(df) %>% 
    step_mutate(pclass = fct_rev(ordered(pclass))) %>% 
    step_mutate(
      family = extrair_familia(name),
      title = factor(extrair_titulo(name))
    ) %>% 
    step_mutate(
      name = extrair_nome(name),
      role = 'id'
    ) %>% 
    step_mutate(
      sex = as_factor(sex) %>% 
        fct_relabel(
          ~case_when(
            . == 'male' ~ 'M',
            . == 'female' ~ 'F',
          )
        )
    ) %>% 
    step_mutate(embarked = as_factor(embarked))
  
  # Se existe faixa, transformar em fator
  if ('faixa' %in% names(df)) {
    rv <- rv %>% step_mutate(faixa = factor(faixa))
  }

  rv
  
}


# Aplicar receita ---------------------------------------------------------

aplicar <- function(receita, df = NULL) {
  
  prep(
    receita, 
    training = df,
    verbose = TRUE,
    retain = TRUE,
    log_changes = TRUE,
    strings_as_factors = FALSE
  ) %>% 
    bake(new_data = NULL)
  
}


# Ler csv e aplicar receita -----------------------------------------------

ler <- function(filename) {
  
  df <- read_csv(filename)
  aplicar(receita_pos_leitura(df))

}


# Todos os níveis de todos os fatores de um df ----------------------------

reunir_niveis <- function(df) {

  nomes <- names(df)
  tipos <- df %>% map(class)
  fatores <- nomes[which(tipos %>% map_lgl(~ 'factor' %in% .x))]
    
  niveis <- fatores %>% 
    map(
      ~ df %>% pull(.x) %>% levels()
    )
  names(niveis) <- fatores
  
  niveis
  
} 
