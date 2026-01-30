# Bibliotecas

library(tidyverse)
library(readxl)

# Bateu a Meta em 2023 (var. resposta) [V] ----

bateu_meta_23 <- read_excel("Bases/METAS E IDEB 2023 - Escolas - Anos Finais e Ensino Médio - VERSÃO FINAL.xlsx",skip = 6) %>%
  select(`Código da Escola`,`Atingiu a Meta\r\nAnos Finais`,`Atingiu a Meta\r\nEnsino Médio`) %>%
  janitor::remove_empty() %>%
  mutate(`Atingiu a Meta\r\nAnos Finais` = case_when(`Atingiu a Meta\r\nAnos Finais` == '-' ~ NA_character_, TRUE ~ `Atingiu a Meta\r\nAnos Finais`),
         `Atingiu a Meta\r\nEnsino Médio` = case_when(`Atingiu a Meta\r\nEnsino Médio` == '-' ~ NA_character_, TRUE ~ `Atingiu a Meta\r\nEnsino Médio`))

# Desafio menor que 0.4 (meta_23 - ideb_21) [V] ----

# IDEB 2021

ideb_21_af <- read_excel("Bases/divulgacao_anos_finais_escolas_2023.xlsx",skip = 9) %>% 
  select("SG_UF","ID_ESCOLA","NO_ESCOLA","REDE","VL_OBSERVADO_2021") %>%
  filter(SG_UF == 'GO',
         REDE == 'Estadual') %>%
  mutate(VL_OBSERVADO_2021 = as.numeric(case_when(VL_OBSERVADO_2021 == '-' ~ NA_character_, TRUE ~ VL_OBSERVADO_2021)))

ideb_21_em <- read_excel("Bases/divulgacao_ensino_medio_escolas_2023.xlsx",skip = 9) %>% 
  select("SG_UF","ID_ESCOLA","NO_ESCOLA","REDE","VL_OBSERVADO_2021") %>%
  filter(SG_UF == 'GO',
         REDE == 'Estadual') %>%
  mutate(VL_OBSERVADO_2021 = as.numeric(case_when(VL_OBSERVADO_2021 == '-' ~ NA_character_, TRUE ~ VL_OBSERVADO_2021)))

# Meta 2023
desafios <- read_excel("Bases/METAS E IDEB 2023 - Escolas - Anos Finais e Ensino Médio - VERSÃO FINAL.xlsx", skip = 6)

desafios <- desafios %>%
  select(`Código da Escola`, `Meta\r\nAnos Finais 2023`,`Meta\r\nEnsino Médio 2023`) %>%
  mutate(`Meta\r\nAnos Finais 2023` = as.numeric(case_when(`Meta\r\nAnos Finais 2023` == '-' ~ NA_character_, TRUE ~ `Meta\r\nAnos Finais 2023`)),
         `Meta\r\nEnsino Médio 2023` = as.numeric(case_when(`Meta\r\nEnsino Médio 2023` == '-' ~ NA_character_, TRUE ~ `Meta\r\nEnsino Médio 2023`))) 

# Juntando
desafios_2023 <- desafios %>%
  left_join(ideb_21_af %>% select(ID_ESCOLA,VL_OBSERVADO_2021), by=c("Código da Escola" = "ID_ESCOLA")) %>%
  left_join(ideb_21_em %>% select(ID_ESCOLA,VL_OBSERVADO_2021), by=c("Código da Escola" = "ID_ESCOLA")) %>%
  rename(IDEB_21_AF = VL_OBSERVADO_2021.x,
         IDEB_21_EM = VL_OBSERVADO_2021.y) %>%
  janitor::remove_empty() %>%
  mutate(Desafio_23_AF = `Meta\r\nAnos Finais 2023` - IDEB_21_AF,
         Desafio_23_EM = `Meta\r\nEnsino Médio 2023` - IDEB_21_EM) %>%
  mutate(Desafio_AF_menor = case_when(Desafio_23_AF >= 0.4 ~ 'NAO',
                                      Desafio_23_AF < 0.4 ~ 'SIM',
                                      TRUE ~ NA_character_),
         Desafio_EM_menor = case_when( Desafio_23_EM >= 0.4  ~ 'NAO',
                                       Desafio_23_EM < 0.4  ~ 'SIM',
                                       TRUE ~ NA_character_))

rm(list=setdiff(ls(),c("bateu_meta_23","desafios_2023")))

# Desafio menor que 0.4 (meta_25 - ideb_23) [V] ----

# IDEB 2023

ideb_23_af <- read_excel("Bases/divulgacao_anos_finais_escolas_2023.xlsx",skip = 9) %>% 
  select("SG_UF","ID_ESCOLA","NO_ESCOLA","REDE","VL_OBSERVADO_2023") %>%
  filter(SG_UF == 'GO',
         REDE == 'Estadual') %>%
  mutate(VL_OBSERVADO_2023 = as.numeric(case_when(VL_OBSERVADO_2023 == '-' ~ NA_character_, TRUE ~ VL_OBSERVADO_2023)))

ideb_23_em <- read_excel("Bases/divulgacao_ensino_medio_escolas_2023.xlsx",skip = 9) %>% 
  select("SG_UF","ID_ESCOLA","NO_ESCOLA","REDE","VL_OBSERVADO_2023") %>%
  filter(SG_UF == 'GO',
         REDE == 'Estadual') %>%
  mutate(VL_OBSERVADO_2023 = as.numeric(case_when(VL_OBSERVADO_2023 == '-' ~ NA_character_, TRUE ~ VL_OBSERVADO_2023)))

# Meta 2025
desafios <- read_excel("Bases/Metas para Ideb 2025 - Versão Final.xlsx", skip = 8)

desafios <- desafios %>%
  select(Cod_Inep, `Meta 2025\r\nAnos Finais`,`Meta 2025\r\nEnsino Médio`) %>%
  mutate(`Meta 2025\r\nAnos Finais` = as.numeric(case_when(`Meta 2025\r\nAnos Finais` == '-' ~ NA_character_, TRUE ~ `Meta 2025\r\nAnos Finais`)),
         `Meta 2025\r\nEnsino Médio` = as.numeric(case_when(`Meta 2025\r\nEnsino Médio` == '-' ~ NA_character_, TRUE ~ `Meta 2025\r\nEnsino Médio`))) 

# Juntando
desafios_2025 <- desafios %>%
  left_join(ideb_23_af %>% select(ID_ESCOLA,VL_OBSERVADO_2023), by=c("Cod_Inep" = "ID_ESCOLA")) %>%
  left_join(ideb_23_em %>% select(ID_ESCOLA,VL_OBSERVADO_2023), by=c("Cod_Inep" = "ID_ESCOLA")) %>%
  rename(IDEB_23_AF = VL_OBSERVADO_2023.x,
         IDEB_23_EM = VL_OBSERVADO_2023.y) %>%
  janitor::remove_empty() %>%
  mutate(Desafio_25_AF = `Meta 2025\r\nAnos Finais` - IDEB_23_AF,
         Desafio_25_EM = `Meta 2025\r\nEnsino Médio` - IDEB_23_EM) %>%
  mutate(Desafio_AF_menor = case_when(Desafio_25_AF >= 0.4 ~ 'NAO',
                                      Desafio_25_AF < 0.4 ~ 'SIM',
                                      TRUE ~ NA_character_),
         Desafio_EM_menor = case_when( Desafio_25_EM >= 0.4  ~ 'NAO',
                                       Desafio_25_EM < 0.4  ~ 'SIM',
                                       TRUE ~ NA_character_))

rm(list=setdiff(ls(),c("bateu_meta_23","desafios_2023","desafios_2025")))

# Crescimento no SAEGO  (2022-2023) [V] ----


# SAEGO 2022
saego_2022 <- read_excel("Bases/resultados_Prg_1655_GOIAS_SAEGO_2022_230113.xlsx",sheet = 16) 

saego_2022_LP_EF <- saego_2022 %>%
  filter(Edição == 2022,
         Etapa == "ENSINO FUNDAMENTAL DE 9 ANOS - 9º ANO",
         Disciplina == "Língua Portuguesa") %>%
  select(`Código escola`,Escola,Proficiência) %>%
  rename(`Proficiencia LP` = Proficiência)

saego_2022_MT_EF <- saego_2022 %>%
  filter(Edição == 2022,
         Etapa == "ENSINO FUNDAMENTAL DE 9 ANOS - 9º ANO",
         Disciplina == "Matemática") %>%
  select(`Código escola`,Proficiência) %>%
  rename(`Proficiencia MT` = Proficiência)

saego_2022_EF <- left_join(saego_2022_LP_EF,saego_2022_MT_EF,by="Código escola")


saego_2022_LP_EM <- saego_2022 %>%
  filter(Edição == 2022,
         Etapa == "ENSINO MEDIO - 3ª SERIE",
         Disciplina == "Língua Portuguesa") %>%
  select(`Código escola`,Escola,Proficiência) %>%
  rename(`Proficiencia LP` = Proficiência)

saego_2022_MT_EM <- saego_2022 %>%
  filter(Edição == 2022,
         Etapa == "ENSINO MEDIO - 3ª SERIE",
         Disciplina == "Matemática") %>%
  select(`Código escola`,Proficiência) %>%
  rename(`Proficiencia MT` = Proficiência)

saego_2022_EM <- left_join(saego_2022_LP_EM,saego_2022_MT_EM,by="Código escola")


# SAEGO 2023
saego_2023 <- read_excel("Bases/resultados_Prg_1732_Saego_2023_231213.xlsx",sheet = 19) 

saego_2023_LP_EF <- saego_2023 %>%
  filter(Edição == 2023,
         Etapa == "ENSINO FUNDAMENTAL DE 9 ANOS - 9º ANO",
         Disciplina == "Língua Portuguesa") %>%
  select(`Código Escola`,Escola,Proficiência) %>%
  rename(`Proficiencia LP` = Proficiência)

saego_2023_MT_EF <- saego_2023 %>%
  filter(Edição == 2023,
         Etapa == "ENSINO FUNDAMENTAL DE 9 ANOS - 9º ANO",
         Disciplina == "Matemática") %>%
  select(`Código Escola`,Proficiência) %>%
  rename(`Proficiencia MT` = Proficiência)

saego_2023_EF <- left_join(saego_2023_LP_EF,saego_2023_MT_EF,by="Código Escola")


saego_2023_LP_EM <- saego_2023 %>%
  filter(Edição == 2023,
         Etapa == "ENSINO MEDIO - 3ª SERIE",
         Disciplina == "Língua Portuguesa") %>%
  select(`Código Escola`,Escola,Proficiência) %>%
  rename(`Proficiencia LP` = Proficiência)

saego_2023_MT_EM <- saego_2023 %>%
  filter(Edição == 2023,
         Etapa == "ENSINO MEDIO - 3ª SERIE",
         Disciplina == "Matemática") %>%
  select(`Código Escola`,Proficiência) %>%
  rename(`Proficiencia MT` = Proficiência)

saego_2023_EM <- left_join(saego_2023_LP_EM,saego_2023_MT_EM,by="Código Escola")

# Crescimento EF
saego_2022e2023_EF_LP <- left_join(
  saego_2023_EF %>% select(-`Proficiencia MT`), 
  saego_2022_EF %>% select(-`Proficiencia MT`,-Escola), 
  by = c("Código Escola" = "Código escola")
) %>% rename(Proficiencia_2022 = `Proficiencia LP.y`,
             Proficiencia_2023 = `Proficiencia LP.x`) %>%
  mutate(
         Proficiencia_2022 = as.numeric(gsub(",", ".", Proficiencia_2022)),
         Proficiencia_2023 = as.numeric(gsub(",", ".", Proficiencia_2023))) %>%
  mutate(Crescimento = (Proficiencia_2023 - Proficiencia_2022))

saego_2022e2023_EF_MT <- left_join(
  saego_2023_EF %>% select(-`Proficiencia LP`), 
  saego_2022_EF %>% select(-`Proficiencia LP`,-Escola), 
  by = c("Código Escola" = "Código escola")
) %>% rename(Proficiencia_2022 = `Proficiencia MT.y`,
             Proficiencia_2023 = `Proficiencia MT.x`) %>%
  mutate(
    Proficiencia_2022 = as.numeric(gsub(",", ".", Proficiencia_2022)),
    Proficiencia_2023 = as.numeric(gsub(",", ".", Proficiencia_2023))) %>%
  mutate(Crescimento = (Proficiencia_2023 - Proficiencia_2022))

# Crescimento EM
saego_2022e2023_EM_LP <- left_join(
  saego_2023_EM %>% select(-`Proficiencia MT`), 
  saego_2022_EM %>% select(-`Proficiencia MT`,-Escola), 
  by = c("Código Escola" = "Código escola")
) %>% rename(Proficiencia_2022 = `Proficiencia LP.y`,
             Proficiencia_2023 = `Proficiencia LP.x`) %>%
  mutate(
    Proficiencia_2022 = as.numeric(gsub(",", ".", Proficiencia_2022)),
    Proficiencia_2023 = as.numeric(gsub(",", ".", Proficiencia_2023))) %>%
  mutate(Crescimento = (Proficiencia_2023 - Proficiencia_2022))

saego_2022e2023_EM_MT <- left_join(
  saego_2023_EM %>% select(-`Proficiencia LP`), 
  saego_2022_EM %>% select(-`Proficiencia LP`,-Escola), 
  by = c("Código Escola" = "Código escola")
) %>% rename(Proficiencia_2022 = `Proficiencia MT.y`,
             Proficiencia_2023 = `Proficiencia MT.x`) %>%
  mutate(
    Proficiencia_2022 = as.numeric(gsub(",", ".", Proficiencia_2022)),
    Proficiencia_2023 = as.numeric(gsub(",", ".", Proficiencia_2023))) %>%
  mutate(Crescimento = (Proficiencia_2023 - Proficiencia_2022))

crescimento_20222023 <- list(saego_2022e2023_EF_LP,
                             saego_2022e2023_EF_MT,
                             saego_2022e2023_EM_LP,
                             saego_2022e2023_EM_MT)

names(crescimento_20222023) <- c("EF_LP","EF_MT","EM_LP","EM_MT")

rm(list=setdiff(ls(),c("bateu_meta_23","desafios_2023","desafios_2025","crescimento_20222023")))

# Crescimento no SAEGO  (2024-2025) [V] ----



# SAEGO 2024
saego_2024 <- read_excel("Bases/resultados_Prg_1893_Saego_Etapa_Institucional_2024_241210.xlsx",sheet = 17, guess_max = Inf) 

saego_2024_LP_EF <- saego_2024 %>%
  filter(Edição == 2024,
         Etapa == "ENSINO FUNDAMENTAL DE 9 ANOS - 9º ANO",
         Disciplina == "Língua Portuguesa",
         `Tipo de oferta` == "Geral") %>%
  select(`Código da escola`,Escola, Proficiência) %>%
  rename(`Proficiencia LP` = Proficiência) %>% 
  distinct()

saego_2024_MT_EF <- saego_2024 %>%
  filter(Edição == 2024,
         Etapa == "ENSINO FUNDAMENTAL DE 9 ANOS - 9º ANO",
         Disciplina == "Matemática",
         `Tipo de oferta` == "Geral") %>%
  select(`Código da escola`,Proficiência) %>%
  rename(`Proficiencia MT` = Proficiência) %>% 
  distinct()

saego_2024_EF <- left_join(saego_2024_LP_EF,saego_2024_MT_EF,by="Código da escola")


saego_2024_LP_EM <- saego_2024 %>%
  filter(Edição == 2024,
         Etapa == "ENSINO MEDIO - 3ª SERIE",
         Disciplina == "Língua Portuguesa",
         `Tipo de oferta` == "Geral") %>%
  select(`Código da escola`,Escola,Proficiência) %>%
  rename(`Proficiencia LP` = Proficiência)

saego_2024_MT_EM <- saego_2024 %>%
  filter(Edição == 2024,
         Etapa == "ENSINO MEDIO - 3ª SERIE",
         Disciplina == "Matemática",
         `Tipo de oferta` == "Geral") %>%
  select(`Código da escola`,Proficiência) %>%
  rename(`Proficiencia MT` = Proficiência)

saego_2024_EM <- left_join(saego_2024_LP_EM,saego_2024_MT_EM,by="Código da escola")


# SAEGO 2025
saego_2025 <- read_excel("Bases/resultados_Prg_2070_Saego_2025_251113.xlsx",sheet = 20, guess_max = Inf) 

saego_2025_LP_EF <- saego_2025 %>%
  filter(Edição == 2025,
         Etapa == "ENSINO FUNDAMENTAL DE 9 ANOS - 9º ANO",
         Disciplina == "Língua Portuguesa",
         `Tipo de oferta` == "Geral") %>%
  select(`Código da escola`,Escola,Proficiência) %>%
  rename(`Proficiencia LP` = Proficiência)

saego_2025_MT_EF <- saego_2025 %>%
  filter(Edição == 2025,
         Etapa == "ENSINO FUNDAMENTAL DE 9 ANOS - 9º ANO",
         Disciplina == "Matemática",
         `Tipo de oferta` == "Geral") %>%
  select(`Código da escola`,Proficiência) %>%
  rename(`Proficiencia MT` = Proficiência)

saego_2025_EF <- left_join(saego_2025_LP_EF,saego_2025_MT_EF,by="Código da escola")


saego_2025_LP_EM <- saego_2025 %>%
  filter(Edição == 2025,
         Etapa == "ENSINO MEDIO - 3ª SERIE",
         Disciplina == "Língua Portuguesa",
         `Tipo de oferta` == "Geral") %>%
  select(`Código da escola`,Escola,Proficiência) %>%
  rename(`Proficiencia LP` = Proficiência)

saego_2025_MT_EM <- saego_2025 %>%
  filter(Edição == 2025,
         Etapa == "ENSINO MEDIO - 3ª SERIE",
         Disciplina == "Matemática",
         `Tipo de oferta` == "Geral") %>%
  select(`Código da escola`,Proficiência) %>%
  rename(`Proficiencia MT` = Proficiência)

saego_2025_EM <- left_join(saego_2025_LP_EM,saego_2025_MT_EM,by="Código da escola")

# Crescimento EF
saego_2024e2025_EF_LP <- left_join(
  saego_2024_EF %>% select(-`Proficiencia MT`), 
  saego_2025_EF %>% select(-`Proficiencia MT`,-Escola), 
  by = "Código da escola"
) %>% rename(Proficiencia_2024 = `Proficiencia LP.y`,
             Proficiencia_2025 = `Proficiencia LP.x`) %>%
  mutate(
    Proficiencia_2024 = as.numeric(gsub(",", ".", Proficiencia_2024)),
    Proficiencia_2025 = as.numeric(gsub(",", ".", Proficiencia_2025))) %>%
  mutate(Crescimento = (Proficiencia_2025 - Proficiencia_2024))

saego_2024e2025_EF_MT <- left_join(
  saego_2024_EF %>% select(-`Proficiencia LP`), 
  saego_2025_EF %>% select(-`Proficiencia LP`,-Escola), 
  by = "Código da escola"
) %>% rename(Proficiencia_2024 = `Proficiencia MT.y`,
             Proficiencia_2025 = `Proficiencia MT.x`) %>%
  mutate(
    Proficiencia_2024 = as.numeric(gsub(",", ".", Proficiencia_2024)),
    Proficiencia_2025 = as.numeric(gsub(",", ".", Proficiencia_2025))) %>%
  mutate(Crescimento = (Proficiencia_2025 - Proficiencia_2024))

# Crescimento EM
saego_2024e2025_EM_LP <- left_join(
  saego_2024_EM %>% select(-`Proficiencia MT`), 
  saego_2025_EM %>% select(-`Proficiencia MT`,-Escola), 
  by = "Código da escola"
) %>% rename(Proficiencia_2024 = `Proficiencia LP.y`,
             Proficiencia_2025 = `Proficiencia LP.x`) %>%
  mutate(
    Proficiencia_2024 = as.numeric(gsub(",", ".", Proficiencia_2024)),
    Proficiencia_2025 = as.numeric(gsub(",", ".", Proficiencia_2025))) %>%
  mutate(Crescimento = (Proficiencia_2025 - Proficiencia_2024))

saego_2024e2025_EM_MT <- left_join(
  saego_2024_EM %>% select(-`Proficiencia LP`), 
  saego_2025_EM %>% select(-`Proficiencia LP`,-Escola), 
  by = "Código da escola"
) %>% rename(Proficiencia_2024 = `Proficiencia MT.y`,
             Proficiencia_2025 = `Proficiencia MT.x`) %>%
  mutate(
    Proficiencia_2024 = as.numeric(gsub(",", ".", Proficiencia_2024)),
    Proficiencia_2025 = as.numeric(gsub(",", ".", Proficiencia_2025))) %>%
  mutate(Crescimento = (Proficiencia_2025 - Proficiencia_2024))

crescimento_20242025 <- list(saego_2024e2025_EF_LP,
                             saego_2024e2025_EF_MT,
                             saego_2024e2025_EM_LP,
                             saego_2024e2025_EM_MT)

names(crescimento_20242025) <- c("EF_LP","EF_MT","EM_LP","EM_MT")


rm(list=setdiff(ls(),c("bateu_meta_23","desafios_2023","desafios_2025","crescimento_20222023","crescimento_20242025")))

# Crescimento Médio maior no IDEB []----
# Taxa de Aprovação []----
