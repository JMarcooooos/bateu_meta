# Bibliotecas

library(tidymodels)
library(tidyverse)
library(stacks)
library(vip)

load("Z:/SUPGEAR/GEAPPE/BASES DE DADOS/05 - Cálculo de Metas/Demanda Surreal/bases_para_modelagem.RData")

# (EF) ----
set.seed(2026)
dados_treino_ef <- dados_treino_ef %>% 
  select(-CD_ESCOLA) %>% 
  filter(!is.na(Y))

cv_folds <- vfold_cv(dados_treino_ef, v = 5, strata = Y)

ctrl_stack <- control_stack_grid() 

# Receita
receita_geral <- recipe(Y ~ ., data = dados_treino_ef) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


# 1. Random Forest
spec_rf <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# 2. XGBoost
spec_xgboost <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),    
  sample_size = tune(),    
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Workflows
wf_rf <- workflow() %>% add_recipe(receita_geral) %>% add_model(spec_rf)
wf_xgb <- workflow() %>% add_recipe(receita_geral) %>% add_model(spec_xgboost)

# Tunagem
doParallel::registerDoParallel() 

# --- Tunagem RF ---
grid_rf <- grid_regular(
  mtry(range = c(1, ncol(dados_treino_ef) - 1)),
  min_n(),
  levels = 5
)

res_rf <- tune_grid(
  wf_rf,
  resamples = cv_folds,
  grid = grid_rf,
  metrics = metric_set(roc_auc),
  control = ctrl_stack
)

# --- Tunagem XGBoost ---
grid_xgb <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), dados_treino_ef),
  learn_rate(),
  size = 20 
)

res_xgb <- tune_grid(
  wf_xgb,
  resamples = cv_folds,
  grid = grid_xgb,
  metrics = metric_set(roc_auc),
  control = ctrl_stack
)

# Construção do Ensemble 
pilha_dados <- stacks() %>%
  add_candidates(res_rf) %>%
  add_candidates(res_xgb)

# Mistura os modelos 
modelo_ensemble <- pilha_dados %>%
  blend_predictions(
    metric = metric_set(roc_auc), 
    penalty = c(0.001, 0.01, 0.1) 
  )

# Ver quem ganhou mais peso
autoplot(modelo_ensemble)
autoplot(modelo_ensemble, type = "weights") 

# Treina o modelo final
modelo_ensemble_final <- modelo_ensemble %>%
  fit_members()

# Previsao Final

previsoes_classe <- predict(modelo_ensemble_final, 
                            new_data = dados_teste_ef, 
                            type = "class")

previsoes_prob <- predict(modelo_ensemble_final, 
                          new_data = dados_teste_ef, 
                          type = "prob")

resultado_final_ef <- dados_teste_ef %>%
  select(CD_ESCOLA) %>%
  bind_cols(previsoes_classe, previsoes_prob)

# Ver distribuição
table(resultado_final_ef$.pred_class)


# (EM) ----
set.seed(2026)
dados_treino_em <- dados_treino_em %>% 
  select(-CD_ESCOLA) %>% 
  filter(!is.na(Y))

cv_folds <- vfold_cv(dados_treino_em, v = 5, strata = Y)

ctrl_stack <- control_stack_grid() 

# Receita
receita_geral <- recipe(Y ~ ., data = dados_treino_em) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


# 1. Random Forest
spec_rf <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# 2. XGBoost
spec_xgboost <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),    
  sample_size = tune(),    
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Workflows
wf_rf <- workflow() %>% add_recipe(receita_geral) %>% add_model(spec_rf)
wf_xgb <- workflow() %>% add_recipe(receita_geral) %>% add_model(spec_xgboost)

# Tunagem
doParallel::registerDoParallel() 

# --- Tunagem RF ---
grid_rf <- grid_regular(
  mtry(range = c(1, ncol(dados_treino_em) - 1)),
  min_n(),
  levels = 5
)

res_rf <- tune_grid(
  wf_rf,
  resamples = cv_folds,
  grid = grid_rf,
  metrics = metric_set(roc_auc),
  control = ctrl_stack
)

# --- Tunagem XGBoost ---
grid_xgb <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), dados_treino_em),
  learn_rate(),
  size = 20 
)

res_xgb <- tune_grid(
  wf_xgb,
  resamples = cv_folds,
  grid = grid_xgb,
  metrics = metric_set(roc_auc),
  control = ctrl_stack
)

# Construção do Ensemble 
pilha_dados <- stacks() %>%
  add_candidates(res_rf) %>%
  add_candidates(res_xgb)

# Mistura os modelos 
modelo_ensemble <- pilha_dados %>%
  blend_predictions(
    metric = metric_set(roc_auc), 
    penalty = c(0.001, 0.01, 0.1) 
  )

# Ver quem ganhou mais peso
autoplot(modelo_ensemble)
autoplot(modelo_ensemble, type = "weights") 

# Treina o modelo final
modelo_ensemble_final <- modelo_ensemble %>%
  fit_members()

# Previsao Final

previsoes_classe <- predict(modelo_ensemble_final, 
                            new_data = dados_teste_em, 
                            type = "class")

previsoes_prob <- predict(modelo_ensemble_final, 
                          new_data = dados_teste_em, 
                          type = "prob")

resultado_final_em <- dados_teste_em %>%
  select(CD_ESCOLA) %>%
  bind_cols(previsoes_classe, previsoes_prob)

# Ver distribuição
table(resultado_final_em$.pred_class)


# Tabelas finais ----

resultado_final_ef
resultado_final_em

save.image("model_completo.RData")

