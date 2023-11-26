
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
  
  recipe(df) %>% 
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
  
}


# Aplicar receita ---------------------------------------------------------

aplicar <- function(receita) {
  
  prep(
    receita, 
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
