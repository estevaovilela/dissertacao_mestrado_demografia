
# Leitura de dados em SQL --------------------------------------------------------

# Script em linguagem SQL para extração de variáveis de interesse da base de dados longitudinal
# SELECT  
# co_pessoa_base, co_pessoa_fisica_08,
# co_uf_08, dt_nascimento_08, tp_sexo_08, tp_cor_raca_08,
# co_entidade_07, co_entidade_08, co_entidade_09, co_entidade_10,
# co_entidade_11, co_entidade_12, co_entidade_13, co_entidade_14,
# co_entidade_15, co_entidade_16, co_entidade_17, co_entidade_18,
# tp_etapa_ensino_07, tp_etapa_ensino_08, tp_etapa_ensino_09, tp_etapa_ensino_10,
# tp_etapa_ensino_11, tp_etapa_ensino_12, tp_etapa_ensino_13, tp_etapa_ensino_14,
# tp_etapa_ensino_15, tp_etapa_ensino_16, tp_etapa_ensino_17, tp_etapa_ensino_18,
# in_concluinte_07, in_concluinte_08, in_concluinte_09, in_concluinte_10,
# in_concluinte_11, in_concluinte_12, in_concluinte_13, in_concluinte_14,
# in_concluinte_15, in_concluinte_16, in_concluinte_17, in_concluinte_18,
# tp_situacao_07, tp_situacao_08, tp_situacao_09, tp_situacao_10,
# tp_situacao_11, tp_situacao_12, tp_situacao_13, tp_situacao_14,
# tp_situacao_15, tp_situacao_16, tp_situacao_17, tp_situacao_18,
# tp_fluxo_07, tp_fluxo_08, tp_fluxo_09, tp_fluxo_10,
# tp_fluxo_11, tp_fluxo_12, tp_fluxo_13, tp_fluxo_14,
# tp_fluxo_15, tp_fluxo_16, tp_fluxo_17, tp_fluxo_18
# FROM fluxo_escolar.fluxo_07_18
# WHERE tp_etapa_ensino_08 = '4' OR tp_etapa_ensino_08 = '15'

# Script em linguagem R ---------------------------------------------------
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)
library(TraMineR)
library(bit64)
library(ggseqplot)
library(colorspace)
library(ggthemes)
library(cluster)
library(WeightedCluster)
library(colorspace)

# Leitura de arquivos -----------------------------------------------------

# leitura de arquivo resultado do script SQL
df_m2008_etapa_4_15 <- fread("../input/matriculados_2008_etapa_4_15.csv")

# Soares, José Francisco; Alves, Maria Teresa G. (2023). 
# "Nível Socioeconômico das escolas brasileiras 
# (banco de dados - versão 10 de janeiro de 2023)". 
# Núcleo de Pesquisas em Desigualdades Escolares (Nupede); 
# Universidade Federal de Minas Gerais.
# Preprint: https://dx.doi.org/10.2139/ssrn.4325674
df_inse <- fread("../input/NSE_ESCOLAS.csv")

# tabela auxiliar com TP_ETAPA_ENSINO e variável auxiliar TP_ETAPA_FLUXO
# descrita em INEP (2017)
df_etapa_ensino <- read_excel("../input/tb_etapas_ensino.xlsx")

df_etapa_ensino_temp <- df_etapa_ensino %>% 
  select(tp_etapa_indicador, etapa_indicador) %>% 
  distinct() %>% 
  arrange(tp_etapa_indicador)

df_etapa_ensino <- df_etapa_ensino %>% 
  mutate(etapa_indicador_fct = factor(tp_etapa_indicador,
                                      levels = df_etapa_ensino_temp$tp_etapa_indicador,
                                      labels = df_etapa_ensino_temp$etapa_indicador))

# etapa máxima permitida para os anos 2009 a 2015
df_etapa_ensino_max <- read_excel("../input/tb_etapas_ensino.xlsx",
                                  sheet = "Planilha4")
# Ano;etapa_max
# 9;1213
# 10;1214
# 11;1215
# 12;1221
# 13;1222
# 14;1223
# 15;1224

# Filtro de nascimento ----------------------------------------------------

# filtrando nascidos entre 1/7/2000 e 30/6/2001
df_m2008_etapa_4_15 <- df_m2008_etapa_4_15 %>% 
  mutate(dia = str_extract(dt_nascimento_08, pattern = "^\\d{2}"),
         mes = str_extract(str_remove(dt_nascimento_08, pattern = "^\\d{2}"), 
                           pattern = "^\\w{3}"),
         ano = str_extract(str_remove(str_remove(dt_nascimento_08, pattern = "^\\d{2}"), 
                                      pattern = "^\\w{3}"), 
                           pattern = "^\\d{4}")) %>%
  mutate(mes = case_when(
    mes == "JAN" ~ 1,
    mes == "FEB" ~ 2,
    mes == "MAR" ~ 3,
    mes == "APR" ~ 4,
    mes == "MAY" ~ 5,
    mes == "JUN" ~ 6,
    mes == "JUL" ~ 7,
    mes == "AUG" ~ 8,
    mes == "SEP" ~ 9,
    mes == "OCT" ~ 10,
    mes == "NOV" ~ 11,
    mes == "DEC" ~ 12
  )) %>% 
  filter((ano == 2000 & mes >= 7) | (ano == 2001 & mes <= 6))

# Formato longer ----------------------------------------------------------

df_coorte <- df_m2008_etapa_4_15 %>% 
  select(co_pessoa_base, co_uf_08, tp_sexo_08, tp_cor_raca_08)

df_tp_etapa <- df_m2008_etapa_4_15 %>% 
  select(co_pessoa_base, 
         starts_with("tp_etapa_ensino")) %>% 
  gather(key = "ano", value = "tp_etapa_ensino", -co_pessoa_base)

df_tp_situacao <- df_m2008_etapa_4_15 %>% 
  select(co_pessoa_base, 
         starts_with("tp_situacao")) %>% 
  gather(key = "ano", value = "tp_situacao", -co_pessoa_base)

df_tp_fluxo <- df_m2008_etapa_4_15 %>% 
  select(co_pessoa_base, 
         starts_with("tp_fluxo")) %>% 
  gather(key = "ano", value = "tp_fluxo", -co_pessoa_base)

# Matrículas inconsistentes -----------------------------------------------
# Falecidos ---------------------------------------------------------------

falecidos <- df_tp_fluxo %>% 
  filter(tp_fluxo == "7") %>% 
  select(co_pessoa_base) %>% 
  bind_rows(df_tp_situacao %>% 
              filter(tp_situacao == "3") %>% 
              select(co_pessoa_base)) %>% 
  distinct()

df_coorte <- df_coorte %>% 
  anti_join(falecidos, by = ("co_pessoa_base"))

# Inconsistências -----------------------------------------

# A coorte é de estudantes nascidos entre 1/7/2000 e 30/6/2001. 
# São selecionados aqueles matriculados na 1ª Série do EF (8 anos) 
# ou no 2º Ano do EF (9 anos).

# Alunos com matrícula no 1º Ano do EF, Pré escola ou Creche - 
alunos_c_inconsistencia_1 <- df_tp_etapa %>% 
  mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))) %>% 
  filter(ano > 7) %>%
  semi_join(df_coorte, by = c("co_pessoa_base")) %>% 
  # NA significa que não tem matrícula
  mutate(tp_etapa_ensino = as.integer(tp_etapa_ensino)) %>% 
  left_join(df_etapa_ensino %>% select(tp_etapa_ensino, 
                                       etapa_indicador_fct), 
            by = c("tp_etapa_ensino")) %>% 
  filter(etapa_indicador_fct %in% c("Creche", "Pré escola", "1º ano E.F.")) %>% 
  select(co_pessoa_base) %>% 
  distinct()

df_coorte <- df_coorte %>% anti_join(alunos_c_inconsistencia_1)

# Alunos com matrícula em EJA E.F. antes de 2016
# Alunos com matrícula em EJA E.M. antes de 2018
alunos_c_inconsistencia_2 <- df_tp_etapa %>% 
  mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))) %>% 
  filter(ano > 7) %>%
  semi_join(df_coorte, by = c("co_pessoa_base")) %>% 
  mutate(tp_etapa_ensino = as.integer(tp_etapa_ensino)) %>% 
  left_join(df_etapa_ensino %>% select(tp_etapa_ensino, 
                                       etapa_indicador_fct), 
            by = c("tp_etapa_ensino")) %>% 
  filter((etapa_indicador_fct == "EJA E.F." & ano <= 15) | 
           (etapa_indicador_fct == "EJA E.M" & ano <= 17)) %>%
  select(co_pessoa_base) %>% 
  distinct()

df_coorte <- df_coorte %>% anti_join(alunos_c_inconsistencia_2)

# Alunos acima da diagonal do diagrama de lexis 
# (etapa de ensino x ano) até 2015
alunos_c_inconsistencia_3 <- df_tp_etapa %>% 
  mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))) %>% 
  filter(ano > 7 & ano <= 15) %>%
  semi_join(df_coorte, by = c("co_pessoa_base")) %>% 
  mutate(tp_etapa_ensino = as.integer(tp_etapa_ensino)) %>% 
  left_join(df_etapa_ensino_max, by = c("ano" = "Ano")) %>% 
  left_join(df_etapa_ensino %>% select(tp_etapa_ensino, 
                                       tp_etapa_indicador), 
            by = c("tp_etapa_ensino")) %>% 
  filter(tp_etapa_indicador > etapa_max) %>% 
  select(co_pessoa_base) %>% 
  distinct()

df_coorte <- df_coorte %>% anti_join(alunos_c_inconsistencia_3)

# Alunos com matrícula na 4ª série do E.M., Não seriado E.M., 
# E.P. Concomitante, E.P. Subsequente
alunos_c_inconsistencia_4 <- df_tp_etapa %>% 
  mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))) %>% 
  filter(ano > 7) %>%
  semi_join(df_coorte, by = c("co_pessoa_base")) %>% 
  mutate(tp_etapa_ensino = as.integer(tp_etapa_ensino)) %>% 
  left_join(df_etapa_ensino %>% select(tp_etapa_ensino, 
                                       etapa_indicador_fct), 
            by = c("tp_etapa_ensino")) %>% 
  filter(etapa_indicador_fct %in% c("4ª série E.M.", "Não seriado E.M.", 
                                    "E.P. Concomitante", "E.P. Subsequente")) %>% 
  select(co_pessoa_base) %>% 
  distinct()

df_coorte <- df_coorte %>% anti_join(alunos_c_inconsistencia_4)

# Alunos acima da diagonal do diagrama de lexis 
# (etapa de ensino x ano) em 2016 ou 2017
alunos_c_inconsistencia_5 <- df_tp_etapa %>% 
  mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))) %>% 
  filter(ano >= 16 & ano <= 17) %>%
  semi_join(df_coorte, by = c("co_pessoa_base")) %>% 
  mutate(tp_etapa_ensino = as.integer(tp_etapa_ensino)) %>% 
  left_join(df_etapa_ensino %>% select(tp_etapa_ensino, 
                                       etapa_indicador_fct), by = c("tp_etapa_ensino")) %>% 
  filter((ano == 16 & (etapa_indicador_fct == "2ª série E.M." | etapa_indicador_fct == "3ª série E.M.")) |
           (ano == 17 & etapa_indicador_fct == "3ª série E.M.")) %>% 
  select(co_pessoa_base) %>% 
  distinct()

df_coorte <- df_coorte %>% anti_join(alunos_c_inconsistencia_5)

# alunos com etapa seguinte inferior à anterior
alunos_c_inconsistencia_6 <- df_tp_etapa %>% 
  mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))) %>% 
  filter(ano > 7) %>% 
  semi_join(df_coorte, by = c("co_pessoa_base")) %>% 
  mutate(tp_etapa_ensino = as.integer(tp_etapa_ensino)) %>% 
  left_join(df_etapa_ensino %>% select(tp_etapa_ensino, 
                                       tp_etapa_indicador), by = c("tp_etapa_ensino")) %>% 
  arrange(co_pessoa_base, ano) %>% 
  mutate(tp_etapa_indicador = ifelse(tp_etapa_indicador == 3200 | 
                                       tp_etapa_indicador == 3300, NA, tp_etapa_indicador)) %>% 
  fill(tp_etapa_indicador) %>% 
  mutate(tp_etapa_indicador_lead = lead(tp_etapa_indicador)) %>%
  filter(ano < 18) %>% 
  mutate(inconsistencia_6 = tp_etapa_indicador > tp_etapa_indicador_lead) %>%  
  filter(inconsistencia_6 == TRUE) %>% 
  select(co_pessoa_base) %>% 
  distinct()

df_coorte <- df_coorte %>% anti_join(alunos_c_inconsistencia_6)

# Inconsitência de fluxo: estudantes que tem fluxo "9 - Não se aplica", 
# mas a etapa não é EJA e as outras opções válidas já foram retiradas 
# INEP (2017) 
# "O fluxo dos alunos não é avaliado para as seguintes 
# etapas (observar valores da variável de etapas auxiliar TP_ETAPA_INDICADOR - ver Anexo 1): 
# Creche (1101), Pré-escola (1102), 4ª série E.M. (1304), Não seriado E.M. (1305), 
# E.P. Concomitante (2001), E.P. Subsequente (2002) (...)")
alunos_c_inconsistencia_7 <- df_tp_fluxo %>% 
  semi_join(df_coorte) %>% 
  mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))) %>% 
  filter(ano > 7 & ano < 18) %>% 
  filter(tp_fluxo == "9") %>% 
  left_join(df_tp_etapa %>% 
              mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))), 
            by = c("co_pessoa_base", "ano")) %>% 
  filter(!tp_etapa_ensino %in% c("65", "69", "70", "73")) %>% 
  select(co_pessoa_base) %>% 
  distinct()

df_coorte <- df_coorte %>% anti_join(alunos_c_inconsistencia_7)

# natrículas na EJA com fluxo diferente de 6 - Migração para o regular
# e 9 - Não se aplica
alunos_c_inconsistencia_8 <- df_tp_etapa %>% 
  mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))) %>% 
  filter(ano > 7 & ano < 18) %>% 
  semi_join(df_coorte, by = c("co_pessoa_base")) %>% 
  mutate(tp_etapa_ensino = as.integer(tp_etapa_ensino)) %>% 
  left_join(df_etapa_ensino %>% select(tp_etapa_ensino, 
                                       tp_etapa_indicador), by = c("tp_etapa_ensino")) %>% 
  filter(tp_etapa_indicador == 3200 | tp_etapa_indicador == 3300) %>% 
  left_join(df_tp_fluxo %>% 
              mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))),
            by = c("co_pessoa_base", "ano")) %>% 
  filter(!tp_fluxo %in% c("6", "9")) %>% 
  select(co_pessoa_base) %>% 
  distinct()

df_coorte <- df_coorte %>% anti_join(alunos_c_inconsistencia_8)

# INSE --------------------------------------------------------------------

df_m2008_etapa_4_15_escolas_longer <- df_coorte_pre %>% 
  mutate(across(starts_with("co_entidade"), as.integer)) %>% 
  pivot_longer(cols = -c(co_pessoa_base, co_uf_08, tp_sexo_08, tp_cor_raca_08),
               names_to = "ano",
               values_to = "cod_escola") %>% 
  mutate(ano = as.integer(str_extract(ano, pattern = "\\d{2}$"))) %>% 
  filter(ano > 7) %>% 
  filter(!is.na(cod_escola))

# 3.109 escolas não possuem INSE
df_m2008_etapa_4_15_escolas_longer %>% 
  select(cod_escola) %>% distinct() %>% 
  anti_join(df_inse,by = c("cod_escola" = "CO_ENTIDADE")) 

df_inse <- df_inse %>% 
  select(CO_ENTIDADE, NSE10) %>% 
  mutate(NSE10 = str_replace(NSE10, pattern = "\\,", replacement = "\\.")) %>% 
  mutate(NSE10 = as.numeric(NSE10))

df_m2008_etapa_4_15_escolas_longer <- df_m2008_etapa_4_15_escolas_longer %>% 
  left_join(df_inse, by = c("cod_escola" = "CO_ENTIDADE")) %>% 
  group_by(co_pessoa_base) %>% 
  summarise(n_escolas = n(),
            n_escolas_sem_nse = sum(is.na(NSE10)),
            nse_medio = mean(NSE10, na.rm = TRUE)) %>% 
  mutate(tipo_nse = case_when(n_escolas_sem_nse == 0 ~ "tipo_1", 
                              # estudantes com NSE para todos os anos em que esteve na escola
                              n_escolas == n_escolas_sem_nse ~ "tipo_2", 
                              # estudantes sem NSE para todos os anos em que esteve na escola
                              TRUE ~ "tipo_3"))

estudantes_sem_nse <- df_m2008_etapa_4_15_escolas_longer %>% 
  filter(tipo_nse == "tipo_2")

df_coorte_pre <- df_coorte_pre %>% 
  left_join(df_m2008_etapa_4_15_escolas_longer %>% 
              select(-n_escolas, -n_escolas_sem_nse), by = c("co_pessoa_base"))

df_coorte <- df_coorte %>% 
  left_join(df_m2008_etapa_4_15_escolas_longer %>% 
              select(-n_escolas, -n_escolas_sem_nse), by = c("co_pessoa_base"))

# 734 estudantes
df_coorte <- df_coorte %>% 
  filter(tipo_nse != "tipo_2")

# Alteração de variáveis - sexo, cor/raça e NSE -------------------------------------

quintis_nse_coorte <- df_coorte %>% 
  select(nse_medio) %>% arrange(nse_medio) %>% pull()

quintis_nse_coorte <- quantile(quintis_nse_coorte, probs = seq(0,1,0.2))

df_coorte <- df_coorte %>% 
  select(co_pessoa_base, co_uf_08, sexo = tp_sexo_08, cor_raca = tp_cor_raca_08,
         nse_medio, tipo_nse) %>% 
  mutate(sexo = factor(sexo,
                       labels = c("Masculino", "Feminino")),
         cor_raca = factor(cor_raca,
                           labels = c("Não declarada", "Branca", "Preta", 
                                      "Parda", "Amarela", "Indígena")),
         tipo_nse = factor(tipo_nse,
                           labels = c("NSE em todas escolas", "NSE em alguma escola")),
         quintil = case_when(
           nse_medio < quintis_nse_coorte[2] ~ "1º Quintil",
           nse_medio < quintis_nse_coorte[3] ~ "2º Quintil",
           nse_medio < quintis_nse_coorte[4] ~ "3º Quintil",
           nse_medio < quintis_nse_coorte[5] ~ "4º Quintil",
           TRUE ~ "5º Quintil",
         ))

# Distribuição da coorte por sexo, cor/raça e NSE ------------------------

# sexo
df_coorte %>% 
  group_by(sexo) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n) * 100, digits = 2))

# cor/raça
df_coorte %>% 
  group_by(cor_raca) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n) * 100, digits = 2))

# NSE - Figura 1
paleta <- RColorBrewer::brewer.pal(5, name = "BrBG")

df_coorte %>% 
  select(nse_medio, quintil) %>% 
  mutate(nse_medio_z_score = (nse_medio - mean(nse_medio)) / sd(nse_medio)) %>% 
  ggplot(aes(x = nse_medio_z_score)) +
  geom_histogram(aes(fill = quintil), color = "black", bins = 100) +
  scale_fill_manual(values = rev(paleta)) +
  theme_clean() +
  labs(x = "NSE padronizado",
       y = "Número de estudantes")

# NSE - Figura 3
df_seq_etapa_sample %>% 
  select(nse_medio, quintil) %>% 
  mutate(nse_medio_z_score = (nse_medio - mean(nse_medio)) / sd(nse_medio)) %>% 
  ggplot(aes(x = nse_medio_z_score)) +
  geom_histogram(aes(fill = quintil), color = "black", bins = 100) +
  scale_fill_manual(values = rev(paleta)) +
  theme_clean() +
  labs(x = "NSE padronizado",
       y = "Número de estudantes")

# Formato Wide - sequência ----------------------------------------------------------

ano_evasao <- df_tp_etapa %>% 
  as_tibble() %>% 
  mutate(tp_etapa_ensino = as.integer(tp_etapa_ensino)) %>% 
  left_join(df_etapa_ensino %>% select(tp_etapa_ensino, 
                                       tp_etapa_indicador), by = c("tp_etapa_ensino")) %>% 
  mutate(tp_etapa_indicador = ifelse(is.na(tp_etapa_indicador), 9999, tp_etapa_indicador)) %>% 
  # evasão considerada incluindo migração para EJA
  filter(tp_etapa_indicador %in% c(3200, 3300, 9999)) %>% 
  select(-tp_etapa_ensino, -tp_etapa_indicador) %>% 
  arrange(co_pessoa_base, ano) %>% 
  group_by(co_pessoa_base) %>% 
  slice(1L)

df_seq_etapa <- df_tp_etapa %>% 
  as_tibble() %>% 
  arrange(co_pessoa_base, ano) %>% 
  mutate(tp_etapa_ensino = as.integer(tp_etapa_ensino)) %>% 
  left_join(df_etapa_ensino %>% select(tp_etapa_ensino, 
                                       tp_etapa_indicador), by = c("tp_etapa_ensino")) %>% 
  mutate(tp_etapa_indicador = ifelse(is.na(tp_etapa_indicador), 9999, tp_etapa_indicador)) %>% 
  left_join(ano_evasao, by = c("co_pessoa_base")) %>% 
  rename(ano_calendario = ano.x,
         ano_evasao = ano.y) %>% 
  mutate(tp_etapa_indicador = case_when(
    !is.na(ano_evasao) & ano_calendario >= ano_evasao ~ 9999,
    TRUE ~ tp_etapa_indicador
  ))

df_seq_etapa <- df_seq_etapa %>% 
  mutate(etapa_indicador_fct = factor(tp_etapa_indicador, 
                                      levels = c(1212, 1213, 1214, 1215, 
                                                 1221, 1222, 1223, 1224,
                                                 1301, 1302, 1303,
                                                 9999),
                                      labels = c("2º ano E.F.", "3º ano E.F.", "4º ano E.F.", "5º ano E.F.",
                                                 "6º ano E.F.", "7º ano E.F.", "8º ano E.F.", "9º ano E.F.",
                                                 "1ª série E.M.", "2ª série E.M.", "3ª série E.M.",
                                                 "Fora da escola"))) %>% 
  select(-tp_etapa_ensino, -tp_etapa_indicador, -ano_evasao) %>% 
  spread(ano_calendario, etapa_indicador_fct) 

# Tratamento de dados -----------------------------------------------------

colnames(df_seq_etapa)[3:13] <- c(2008:2018)

df_seq_etapa <- df_seq_etapa%>% 
  left_join(df_coorte, by = c("co_pessoa_base"))

# Cria objeto no formato sequência com pacote TraMineR --------------------

df_seq_etapa <- df_seq_etapa %>% 
  mutate(quintil = factor(quintil,
                          levels = c("1º Quintil", "2º Quintil", 
                                     "3º Quintil", "4º Quintil", "5º Quintil")))

# etapa
etapa <- c("2º ano E.F.", "3º ano E.F.", "4º ano E.F.", "5º ano E.F.",
           "6º ano E.F.", "7º ano E.F.", "8º ano E.F.", "9º ano E.F.",
           "1ª série E.M.", "2ª série E.M.", "3ª série E.M.", "Fora da escola")

paleta <- RColorBrewer::brewer.pal(11, name = "BrBG")
paleta <- c("#C44747", paleta)

seq_etapa <- TraMiner::seqdef(df_seq_etapa, 3:13, alphabet = etapa, informat = "STS")

# Distribuição de estados - tabelas ---------------------------------------

# TraMineR::seqstatd
# Sequence of transversal state distributions and their entropies
# Returns the state relative frequencies, the number of valid states and the 
# entropy of the state distribution at each position in the sequence.

# State distribution table
state_distribution_table <- seqstatd(seq_etapa)

state_distribution_table <- tibble(as.data.frame(state_distribution_table$Frequencies)) %>% 
  mutate(estado = factor(etapa,
                         levels = etapa)) %>% 
  select(estado, everything())

# sexo
state_distribution_table_sexo <- tibble()
sexo <- c("Feminino", "Masculino")

for(i in 1:2) {
  
  state_distribution_table <- seqstatd(seq_etapa[df_seq_etapa$sexo == sexo[i],])
  
  temp <- tibble(as.data.frame(state_distribution_table$Frequencies)) %>% 
    mutate(estado = factor(etapa,
                           levels = etapa)) %>% 
    mutate(sexo = sexo[i]) %>% 
    select(sexo, estado, everything())
  
  state_distribution_table_sexo <- state_distribution_table_sexo %>% 
    bind_rows(temp)
  
}

# cor/raça
state_distribution_table_cor_raca <- tibble()
cor_raca <- c("Não declarada", "Branca", "Preta", "Parda", "Amarela", "Indígena")

for(i in 1:6) {
  
  state_distribution_table <- seqstatd(seq_etapa[df_seq_etapa$cor_raca == cor_raca[i],])
  
  temp <- tibble(as.data.frame(state_distribution_table$Frequencies)) %>% 
    mutate(estado = factor(etapa,
                           levels = etapa)) %>% 
    mutate(cor_raca = cor_raca[i]) %>% 
    select(cor_raca, estado, everything())
  
  state_distribution_table_cor_raca <- state_distribution_table_cor_raca %>% 
    bind_rows(temp)
  
}

# quintil
state_distribution_table_quintil <- tibble()
quintil <- c("1º Quintil", "2º Quintil", "3º Quintil", "4º Quintil", "5º Quintil")

for(i in 1:5) {
  
  state_distribution_table <- seqstatd(seq_etapa[df_seq_etapa$quintil == quintil[i],])
  
  temp <- tibble(as.data.frame(state_distribution_table$Frequencies)) %>% 
    mutate(estado = factor(etapa,
                           levels = etapa)) %>% 
    mutate(quintil = quintil[i]) %>% 
    select(quintil, estado, everything())
  
  state_distribution_table_quintil <- state_distribution_table_quintil %>% 
    bind_rows(temp)
  
}

# Distribuição de estados - gráficos --------------------------------------

# Figura 4
state_distribution_table %>% 
  pivot_longer(cols = -estado,
               names_to = "ano",
               values_to = "frequência") %>% 
  filter(frequência > 0) %>% 
  mutate(frequência = frequência * 100) %>% 
  ggplot(aes(x = ano, y = frequência, fill = estado)) +
  geom_col(color = "black") +
  scale_fill_manual(values = rev(paleta)) +
  scale_y_continuous(limits = c(0,100.1), breaks = seq(0,100,10)) +
  labs(x = "Ano",
       y = "Distribuição (%)") +
  theme_clean()

# Figura 6
state_distribution_table_sexo %>% 
  pivot_longer(cols = -c(sexo, estado),
               names_to = "ano",
               values_to = "frequência") %>% 
  filter(frequência > 0) %>% 
  mutate(frequência = frequência * 100) %>% 
  ggplot(aes(x = ano, y = frequência, fill = estado)) +
  geom_col(color = "black") +
  scale_fill_manual(values = rev(paleta)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  labs(x = "Ano",
       y = "Distribuição (%)") +
  theme_clean() +
  theme(legend.position = "bottom") +
  facet_wrap(~ sexo)

# Figura 8
state_distribution_table_cor_raca %>% 
  pivot_longer(cols = -c(cor_raca, estado),
               names_to = "ano",
               values_to = "frequência") %>% 
  filter(frequência > 0) %>% 
  mutate(frequência = frequência * 100) %>% 
  ggplot(aes(x = ano, y = frequência, fill = estado)) +
  geom_col(color = "black") +
  scale_fill_manual(values = rev(paleta)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  labs(x = "Ano",
       y = "Distribuição (%)") +
  theme_clean() +
  theme(legend.position = "bottom") +
  facet_wrap(~ cor_raca)

# Figura 10
state_distribution_table_quintil %>% 
  pivot_longer(cols = -c(quintil, estado),
               names_to = "ano",
               values_to = "frequência") %>% 
  filter(frequência > 0) %>% 
  mutate(frequência = frequência * 100) %>% 
  ggplot(aes(x = ano, y = frequência, fill = estado)) +
  geom_col(color = "black") +
  scale_fill_manual(values = rev(paleta)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  labs(x = "Ano",
       y = "Distribuição (%)") +
  theme_clean() +
  theme(legend.position = "bottom") +
  facet_wrap(~ quintil)

# 10 sequências mais comuns - tabelas -------------------------------------

# TraMineR::seqtab
# Frequency table of the sequences
# Computes the frequency table of the sequences (count and percent of each sequence).

sequency_frequency_table <- seqtab(seq_etapa)

sequency_frequency_table <- tibble(sequency_frequency_table[1:10,]) %>% 
  mutate(id = seq(1,10)) %>% 
  mutate(frequencia = attr(sequency_frequency_table, "freq")$Freq,
         percentual = attr(sequency_frequency_table, "freq")$Percent) %>% 
  select(id, frequencia, percentual, everything())

# sexo
sequency_frequency_table_sexo <- tibble()

for(i in 1:2) {
  
  sequency_frequency_table <- seqtab(seq_etapa[df_seq_etapa$sexo == sexo[i],])
  
  temp <- tibble(sequency_frequency_table[1:10,]) %>% 
    mutate(id = seq(1,10,1)) %>% 
    mutate(frequencia = attr(sequency_frequency_table, "freq")$Freq,
           percentual = attr(sequency_frequency_table, "freq")$Percent) %>% 
    mutate(sexo = sexo[i]) %>% 
    select(sexo, id, frequencia, percentual, everything())
  
  sequency_frequency_table_sexo <- sequency_frequency_table_sexo %>% 
    bind_rows(temp)
  
}

# cor/raça
sequency_frequency_table_cor_raca <- tibble()

for(i in 1:6) {
  
  sequency_frequency_table <- seqtab(seq_etapa[df_seq_etapa$cor_raca == cor_raca[i],])
  
  temp <- tibble(sequency_frequency_table[1:10,]) %>% 
    mutate(id = seq(1,10,1)) %>% 
    mutate(frequencia = attr(sequency_frequency_table, "freq")$Freq,
           percentual = attr(sequency_frequency_table, "freq")$Percent) %>% 
    mutate(cor_raca = cor_raca[i]) %>% 
    select(cor_raca, id, frequencia, percentual, everything())
  
  sequency_frequency_table_cor_raca <- sequency_frequency_table_cor_raca %>% 
    bind_rows(temp)
  
}

# quintil
sequency_frequency_table_quintil <- tibble()

for(i in 1:5) {
  
  sequency_frequency_table <- seqtab(seq_etapa[df_seq_etapa$quintil == quintil[i],])
  
  temp <- tibble(sequency_frequency_table[1:10,]) %>% 
    mutate(id = seq(1,10,1)) %>% 
    mutate(frequencia = attr(sequency_frequency_table, "freq")$Freq,
           percentual = attr(sequency_frequency_table, "freq")$Percent) %>% 
    mutate(quintil = quintil[i]) %>% 
    select(quintil, id, frequencia, percentual, everything())
  
  sequency_frequency_table_quintil <- sequency_frequency_table_quintil %>% 
    bind_rows(temp)
  
}

# 10 sequências mais comuns - gráficos ------------------------------------

# Figura 5
sequency_frequency_table %>% 
  pivot_longer(cols = -c(id, frequencia, percentual),
               names_to = "ano",
               values_to = "estado") %>% 
  ggplot(aes(x = ano, y = percentual, fill = estado, group = rev(id))) +
  geom_tile() +
  geom_col(color = "black") +
  scale_fill_manual(values = rev(paleta)) +
  scale_y_continuous(limits = c(0,100.1), breaks = seq(0,100,10)) +
  labs(x = "Ano",
       y = "Frequência (%)") +
  theme_clean()

# Figura 7
sequency_frequency_table_sexo %>% 
  pivot_longer(cols = -c(sexo, id, frequencia, percentual),
               names_to = "ano",
               values_to = "estado") %>% 
  ggplot(aes(x = ano, y = percentual, fill = estado, group = rev(id))) +
  geom_tile() +
  geom_col(color = "black") +
  scale_fill_manual(values = rev(paleta)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  labs(x = "Ano",
       y = "Frequência (%)") +
  theme_clean() +
  theme(legend.position = "bottom") +
  facet_wrap(~ sexo)

# Figura 9
sequency_frequency_table_cor_raca %>% 
  pivot_longer(cols = -c(cor_raca, id, frequencia, percentual),
               names_to = "ano",
               values_to = "estado") %>% 
  ggplot(aes(x = ano, y = percentual, fill = estado, group = rev(id))) +
  geom_tile() +
  geom_col(color = "black") +
  scale_fill_manual(values = rev(paleta)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  labs(x = "Ano",
       y = "Frequência (%)") +
  theme_clean() +
  theme(legend.position = "bottom") +
  facet_wrap(~ cor_raca)

# Figura 11
sequency_frequency_table_quintil %>% 
  pivot_longer(cols = -c(quintil, id, frequencia, percentual),
               names_to = "ano",
               values_to = "estado") %>% 
  ggplot(aes(x = ano, y = percentual, fill = estado, group = rev(id))) +
  geom_tile() +
  geom_col(color = "black") +
  scale_fill_manual(values = rev(paleta)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  labs(x = "Ano",
       y = "Frequência (%)") +
  theme_clean() +
  theme(legend.position = "bottom") +
  facet_wrap(~ quintil)

# Aplica o método OM1&2 ---------------------------------------------------

# matriz de custos
couts <- seqsubm(seq_etapa, method = "CONSTANT")

set.seed(123)
df_seq_etapa_sample <- df_seq_etapa %>% 
  sample_n(46341)

seq_etapa_sample <- seqdef(df_seq_etapa_sample, 3:13, labels = etapa)

# computa a matriz de dissimilaridade entre os pares de sequências
seq_etapa_sample.om <- seqdist(seq_etapa_sample, 
                               method = "OM", 
                               indel = 1.0, 
                               sm = couts)

object.size(seq_etapa_sample.om)/(1024^2)

start.time <- Sys.time()
# aplica o agrupamento aglomerativo hierário 
clusterward <- cluster::agnes(seq_etapa_sample.om, diss = TRUE, method = "ward")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

wardTree <- WeightedCluster::as.seqtree(clusterward, seqdata = seq_etapa_sample, diss = seq_etapa_sample.om, ncluster = 12)

wardRange <- WeightedCluster::as.clustrange(clusterward, diss=seq_etapa_sample.om, ncluster = 12)

# Seleciona 6 agrupamentos ------------------------------------------------

cluster6 <- cutree(clusterward, k = 6)
cluster6 <- factor(cluster6, labels = c("evasão precoce", 
                                        "regular", 
                                        "pouca irregularidade", 
                                        "evasão - anos finais", 
                                        "evasão - ensino médio", 
                                        "evasão - anos iniciais"))

df_seq_etapa_sample <- df_seq_etapa_sample %>% 
  mutate(cluster = cluster6)

# Distribuição de estados - Padrões de trajetórias ------------------------

state_distribution_table_cluster <- tibble()

for(i in 1:6) {
  
  state_distribution_table <- seqstatd(seq_etapa_sample[df_seq_etapa_sample$cluster == levels(cluster6)[i],])
  
  temp <- tibble(as.data.frame(state_distribution_table$Frequencies)) %>% 
    mutate(estado = factor(etapa,
                           levels = etapa)) %>% 
    mutate(cluster = levels(cluster6)[i]) %>% 
    select(cluster, estado, everything())
  
  state_distribution_table_cluster <- state_distribution_table_cluster %>% 
    bind_rows(temp)
  
}

# Figura 12
ggseqdplot(seq_etapa_sample, group = df_seq_etapa_sample$cluster, border = TRUE) +
  scale_fill_manual(values = rev(paleta)) +
  scale_x_discrete(labels = seq(2008,2018,1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
  labs(x = "Ano",
       y = "Frequência relativa") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, size = 7),
        legend.position = "bottom")

# Tempo médio - Padrões de trajetórias ------------------------------------

# TraMineR::seqmeant
# Mean durations in each state
# Compute the mean total time spent in each state of the alphabet for the set of sequences given as input.

mean_time_table_cluster <- tibble()

for(i in 1:6) {
  
  mean_time_table <- seqmeant(seq_etapa_sample[df_seq_etapa_sample$cluster == levels(cluster6)[i],])
  
  temp <- tibble(estado = rownames(mean_time_table),
                 tempo_medio = mean_time_table[1:12]) %>% 
    mutate(estado = factor(estado,
                           levels = etapa)) %>% 
    mutate(cluster = levels(cluster6)[i]) %>% 
    pivot_wider(id_cols = cluster, names_from = estado, values_from = tempo_medio)
  
  mean_time_table_cluster <- mean_time_table_cluster %>% 
    bind_rows(temp)
  
}

# Figura 13
ggseqmtplot(seq_etapa_sample, group = df_seq_etapa_sample$cluster, border = TRUE) +
  scale_fill_manual(values = paleta) +
  geom_hline(yintercept = 1, color = "red") +
  scale_y_continuous(breaks = seq(0,10,1)) +
  scale_x_discrete( labels = seq(2008,2018,1)) +
  labs(x = "",
       y = "Tempo médio (anos)") +
  theme(axis.text.x = element_blank())

# 10 sequências mais comuns - Padrões de trajetórias ----------------------

sequency_frequency_table_cluster <- tibble()

for(i in 1:6) {
  
  sequency_frequency_table <- seqtab(seq_etapa_sample[df_seq_etapa_sample$cluster == levels(cluster6)[i],])
  
  temp <- tibble(sequency_frequency_table[1:10,]) %>% 
    mutate(id = seq(1,10,1)) %>% 
    mutate(frequencia = attr(sequency_frequency_table, "freq")$Freq,
           percentual = attr(sequency_frequency_table, "freq")$Percent) %>% 
    mutate(cluster = levels(cluster6)[i]) %>% 
    select(cluster, id, frequencia, percentual, everything())
  
  sequency_frequency_table_cluster <- sequency_frequency_table_cluster %>% 
    bind_rows(temp)
  
}

# Figura 14
ggseqfplot(seq_etapa_sample, group = df_seq_etapa_sample$cluster, border = TRUE) +
  scale_fill_manual(values = paleta) +
  scale_x_discrete(labels = seq(2008,2018,1)) +
  labs(x = "Ano",
       y = "Frequência relativa") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, size = 7),
        legend.position = "bottom")

# Estatística descritiva --------------------------------------------------

df_seq_etapa_sample <- df_seq_etapa_sample %>% 
  mutate(cluster = factor(cluster,
                          levels = c("regular",
                                     "pouca irregularidade",
                                     "evasão - anos finais",
                                     "evasão - ensino médio",
                                     "evasão - anos iniciais",
                                     "evasão precoce")),
         cor_raca = factor(cor_raca,
                           levels = c("Parda",
                                      "Branca", 
                                      "Não declarada",
                                      "Preta",
                                      "Indígena",
                                      "Amarela")))

# Tabela 7
df_seq_etapa_sample %>% 
  mutate(variavel = sexo) %>% 
  group_by(cluster, variavel) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  pivot_wider(id_cols = variavel,
              names_from = cluster,
              values_from = c(n, prop))

# Tabela 7
df_seq_etapa_sample %>% 
  mutate(variavel = cor_raca) %>% 
  group_by(cluster, variavel) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  pivot_wider(id_cols = variavel,
              names_from = cluster,
              values_from = c(n, prop))

# Tabela 7
df_seq_etapa_sample %>% 
  mutate(variavel = quintil) %>% 
  group_by(cluster, variavel) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  pivot_wider(id_cols = variavel,
              names_from = cluster,
              values_from = c(n, prop))

# Teste qui-quadrado de independência de proporções -----------------------

# Tabela 8
stats::chisq.test(df_seq_etapa_sample$sexo, df_seq_etapa_sample$cluster)

stats::chisq.test(df_seq_etapa_sample$cor_raca, df_seq_etapa_sample$cluster)

stats::chisq.test(df_seq_etapa_sample$quintil, df_seq_etapa_sample$cluster)

# Apêndice - Seleção do número de agrupamentos ---------------------------

# Figura 15 - Apêndice
seqtreedisplay(wardTree, type="mt", border = NA, show.depth = TRUE, gvpath = "C:/Program Files/GraphViz",
               cpal = paleta, ylim=c(0,10), with.legend = FALSE)

# Figura 16 - Apêndice
tibble(as.data.frame(wardRange$stats)) %>% 
  mutate(n_agrupamentos = seq(2,12)) %>% 
  pivot_longer(cols = -n_agrupamentos,
               names_to = "métrica",
               values_to = "valor") %>% 
  filter(métrica %in% c("ASW", "HG", "PBC", "HC")) %>% 
  ggplot(aes(x = n_agrupamentos, y = valor, group = métrica)) +
  geom_line(aes(color = métrica)) +
  geom_point(aes(color = métrica)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.05)) +
  scale_x_continuous(limits = c(2,12), breaks = seq(2,12,1)) +
  labs(x = "Número de agrupamentos",
       y = "Valor") +
  theme_clean()
