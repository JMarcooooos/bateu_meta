# Bibliotecas

library(tidyverse)
library(readxl)

# Taxa de Aprovação []----

# Bateu a Meta em 2023 [V] ----

bateu_meta_23 <- read_excel("Bases/METAS E IDEB 2023 - Escolas - Anos Finais e Ensino Médio - VERSÃO FINAL.xlsx",skip = 6) %>%
  select(`Código da Escola`,`Atingiu a Meta\r\nAnos Finais`,`Atingiu a Meta\r\nEnsino Médio`) %>%
  janitor::remove_empty() %>%
  mutate(`Atingiu a Meta\r\nAnos Finais` = case_when(`Atingiu a Meta\r\nAnos Finais` == '-' ~ NA_character_, TRUE ~ `Atingiu a Meta\r\nAnos Finais`),
         `Atingiu a Meta\r\nEnsino Médio` = case_when(`Atingiu a Meta\r\nEnsino Médio` == '-' ~ NA_character_, TRUE ~ `Atingiu a Meta\r\nEnsino Médio`))

# Desafio menor que 0.4 [] ----

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

rm(list=setdiff(ls(),"crescimento_20222023","bateu_meta_23"))

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

rm(list=setdiff(ls(),c("crescimento_20222023","crescimento_20242025","bateu_meta_23")))


# Crescimento Médio maior no IDEB []----