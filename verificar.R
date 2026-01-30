# 1. Fazer a previsão nos dados de TREINO (onde sabemos a resposta Y_num)
validacao <- dados_prontos_stan %>%
  add_epred_draws(modelo_bayes, ndraws = 500, allow_new_levels = TRUE) %>% # 500 é suficiente p/ ser rápido
  group_by(NM_MUNICIPIO, NM_REGIONAL) %>% # Agrupando por chaves originais
  summarise(
    Prob_Media = mean(.epred, na.rm=TRUE),
    Y_Real = mean(Y_num), # O valor real (0 ou 1)
    .groups = "drop"
  )

# 2. Aplicar as mesmas réguas de corte que definimos antes
# Lembrando: Top 43% são Verdes
corte_verde_val <- quantile(validacao$Prob_Media, probs = (1 - 0.43))
corte_vermelho_val <- quantile(validacao$Prob_Media, probs = 0.25)

validacao_classificada <- validacao %>%
  mutate(
    CLASSIFICACAO = case_when(
      Prob_Media >= corte_verde_val ~ "VERDE",
      Prob_Media <= corte_vermelho_val ~ "VERMELHO",
      TRUE ~ "AMARELO"
    )
  )

# 3. CALCULAR A "CERTEZA" (A Taxa de Acerto)
tabela_certeza <- validacao_classificada %>%
  group_by(CLASSIFICACAO) %>%
  summarise(
    Total_Escolas = n(),
    Escolas_que_Bateram = sum(Y_Real),
    # ESSA É A PORCENTAGEM QUE VOCÊ VAI FALAR:
    TAXA_DE_CERTEZA = mean(Y_Real) 
  )

print(tabela_certeza)

library(ggplot2)

# Gráfico de barras da Taxa de Sucesso Real por Grupo
ggplot(tabela_certeza, aes(x = CLASSIFICACAO, y = TAXA_DE_CERTEZA, fill = CLASSIFICACAO)) +
  geom_col() +
  geom_text(aes(label = scales::percent(TAXA_DE_CERTEZA, accuracy = 1)), 
            vjust = -0.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) +
  scale_fill_manual(values = c("AMARELO" = "#F4C430", "VERDE" = "#228B22", "VERMELHO" = "#B22222")) +
  labs(
    title = "Confiabilidade do Modelo Predictivo",
    subtitle = "Qual a % real de escolas que bateram a meta em cada grupo classificado?",
    x = "Classificação do Modelo",
    y = "Taxa Histórica de Sucesso (Certeza)"
  ) +
  theme_minimal()




lista_aprovadas <- resultado_calibrado %>%
  filter(grepl("VERDE", CLASSIFICACAO)) %>% 
  select(CD_ESCOLA, NM_MUNICIPIO, NM_REGIONAL, Prob_Media) %>%
  arrange(desc(Prob_Media)) # As mais garantidas no topo


lista_incertas <- resultado_calibrado %>%
  filter(grepl("AMARELO", CLASSIFICACAO)) %>% 
  select(CD_ESCOLA, NM_MUNICIPIO, NM_REGIONAL, Prob_Media) %>%
  arrange(desc(Prob_Media)) # As mais garantidas no topo

lista_naoatingirao <- resultado_calibrado %>%
  filter(grepl("VERMELHO", CLASSIFICACAO)) %>% 
  select(CD_ESCOLA, NM_MUNICIPIO, NM_REGIONAL, Prob_Media) %>%
  arrange(desc(Prob_Media)) # As mais garantidas no topo