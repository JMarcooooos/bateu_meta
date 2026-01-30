# Bayesiano 

library(brms)
library(tidyverse)
library(tidybayes)

load("Z:/SUPGEAR/GEAPPE/BASES DE DADOS/05 - Cálculo de Metas/Demanda Surreal/bases_para_modelagem.RData")

# ENSINO FUNDAMENTAL ----

# Colocando a Regional e Municipio de cada Escola
dados_regmun <- readxl::read_excel("Bases/Metas para Ideb 2025 - Versão Final.xlsx",skip=8) %>%
  select(Regional, Município, Cod_Inep)

dados_treino_ef <- dados_treino_ef %>%
  left_join(dados_regmun,by=c("CD_ESCOLA" = "Cod_Inep"))

names(dados_treino_ef)[c(8,9)] <- c("NM_REGIONAL","NM_MUNICIPIO")

dados_teste_ef <- dados_teste_ef %>%
  left_join(dados_regmun,by=c("CD_ESCOLA" = "Cod_Inep"))
names(dados_teste_ef)[c(7,8)] <- c("NM_REGIONAL","NM_MUNICIPIO")

# Dados Bayes
dados_bayes_ef <- dados_treino_ef %>%
  select(-CD_ESCOLA) %>%
  filter(!is.na(Y)) %>%
  mutate(Y_num = ifelse(Y == "Sim" | Y == "1", 1, 0)) %>%
  select(-Y) 

dados_bayes_ef <- na.omit(dados_bayes_ef)

# Receita 
receita_escala <- recipe(Y_num ~ ., data = dados_bayes_ef) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), -NM_MUNICIPIO, -NM_REGIONAL) %>%
  step_rename_at(all_predictors(), fn = ~ make.names(.)) %>% 
  prep()

dados_prontos_stan <- bake(receita_escala, new_data = dados_bayes_ef)
dados_teste_proc   <- bake(receita_escala, new_data = dados_teste_ef %>% mutate(Y_num=0))

# Treinar Modelo
modelo_bayes <- brm(
  formula = bf(Y_num ~ . -NM_REGIONAL -NM_MUNICIPIO + (1 | NM_REGIONAL) + (1 | NM_MUNICIPIO), 
               family = bernoulli(link = "logit")),
  data = dados_prontos_stan,
  # backend = "cmdstanr",  <--- (rstan padrão)
  chains = 4,
  cores = 4, 
  # threads = threading(2), <--- (Exclusivo do cmdstanr)
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 2026,
  file = "modelo_bayes_ef_rstan" 
)

dados_teste_final <- dados_teste_proc %>%
  mutate(
    DESAFIO_AF_MENOR_new = 0,
    DESAFIO_AF_MENOR_unknown = 0
  )

# Previsao
previsoes_finais <- dados_teste_final %>%
  mutate(CD_ESCOLA = dados_teste_ef$CD_ESCOLA) %>% 
  add_epred_draws(
    modelo_bayes, 
    ndraws = 1000, 
    allow_new_levels = TRUE
  ) %>% 
  group_by(CD_ESCOLA) %>%
  summarise(
    Prob_Media = mean(.epred, na.rm = TRUE),
    Prob_Min_Credivel = quantile(.epred, 0.05, na.rm = TRUE), 
    Prob_Max_Credivel = quantile(.epred, 0.95, na.rm = TRUE),
    Certeza_Alta = mean(.epred > 0.6, na.rm = TRUE) 
  ) %>%
  arrange(desc(Prob_Media))

View(previsoes_finais)

summary(previsoes_finais$Prob_Media)
hist(previsoes_finais$Prob_Media, main = "Distribuição das Probabilidades", col = "skyblue")

corte_verde <- quantile(previsoes_finais$Prob_Media, probs = (1 - 0.43))
corte_vermelho <- quantile(previsoes_finais$Prob_Media, probs = 0.25)

resultado_calibrado <- previsoes_finais %>%
  mutate(
    CLASSIFICACAO = case_when(
      # Se a escola está no grupo das Top 43% melhores
      Prob_Media >= corte_verde ~ "VERDE: Tendência Alta (No padrão histórico)",
      
      # Se está abaixo do corte vermelho
      Prob_Media <= corte_vermelho ~ "VERMELHO: Risco Crítico (Abaixo do padrão)",
      
      # O resto é o meio de campo
      TRUE ~ "AMARELO: Incerto / Zona de Batalha"
    )
  ) %>%
  left_join(
    dados_teste_ef %>% select(CD_ESCOLA, NM_REGIONAL, NM_MUNICIPIO),
    by = "CD_ESCOLA"
  ) %>%
  select(CD_ESCOLA, NM_REGIONAL, NM_MUNICIPIO, Prob_Media, CLASSIFICACAO) %>%
  arrange(desc(Prob_Media))

cat("Probabilidade mínima para ser VERDE:", round(corte_verde, 3), "\n")
cat("Probabilidade máxima para ser VERMELHO:", round(corte_vermelho, 3), "\n\n")

table(resultado_calibrado$CLASSIFICACAO)
prop.table(table(dados_bayes_ef$Y_num))
#     0         1 
# 0.5718563 0.4281437

# Diagnostico Modelo ----

plot(modelo_bayes)

pp_check(modelo_bayes, ndraws = 100)

conditional_effects(modelo_bayes)

summary(modelo_bayes)

shinystan::launch_shinystan(modelo_bayes)

save.image("modelo_bayesiano.RData")
