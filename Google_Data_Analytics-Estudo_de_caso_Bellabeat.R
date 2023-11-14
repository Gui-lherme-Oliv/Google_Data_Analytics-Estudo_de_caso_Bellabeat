#Instalando os pacotes e abrindo as bibliotecas
install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("lubridate")
install.packages("plotly")

library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)
library(plotly)

#Importando os conjuntos de dados
atividadeDia <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged_20160312-20160512.csv")
caloriasHora <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged_20160312-20160512.csv")
intensidadeHora <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged_20160312-20160512.csv")
passosHora <- read.csv ("~/FitBit/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged_20160312-20160512.csv")
peso <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged_20160312-20160512.csv")
sonoDia <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/sleepDay_merged_20160312-20160512.csv")

#Visualizando e consultando o resumo das medidas estatísticas dos conjuntos de dados
view(atividadeDia)
summary(atividadeDia)

view(caloriasHora)
summary(caloriasHora)

view(intensidadeHora)
summary(intensidadeHora)

view(passosHora)
summary(passosHora)

view(sonoDia)
summary(sonoDia)

view(peso)
summary(peso)

#Verificando a quantidade de usuários distintos
n_distinct(atividadeDia$Id)
n_distinct(caloriasHora$Id)
n_distinct(intensidadeHora$Id)
n_distinct(passosHora$Id)
n_distinct(sonoDia$Id)
n_distinct(peso$Id)

#Verificando a quantidade de valores N/A
sum(is.na(atividadeDia))
sum(is.na(caloriasHora))
sum(is.na(intensidadeHora))
sum(is.na(passosHora))
sum(is.na(sonoDia))

#Verificando e removendo duplicatas
sum(duplicated(atividadeDia))
sum(duplicated(caloriasHora))
sum(duplicated(intensidadeHora))
sum(duplicated(passosHora))
sum(duplicated(sonoDia))

sonoDia <- sonoDia %>%
  distinct()

#Renomenado as colunas
atividadeDia <- atividadeDia %>% 
  clean_names()

caloriasHora <- caloriasHora %>% 
  clean_names()

intensidadeHora <- intensidadeHora %>% 
  clean_names()

passosHora <- passosHora %>% 
  clean_names()

sonoDia <- sonoDia %>% 
  clean_names()

#Correção e consistência das colunas de data e hora
atividadeDia$activity_date=as.POSIXct(atividadeDia$activity_date, format="%m/%d/%Y", tz=Sys.timezone())
atividadeDia$data <- format(atividadeDia$activity_date, format = "%d/%m/%Y")

caloriasHora$activity_hour=as.POSIXct(caloriasHora$activity_hour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
caloriasHora$data <- format(caloriasHora$activity_hour, format = "%d/%m/%Y")
caloriasHora$hora <- format(caloriasHora$activity_hour, format = "%H:%M:%S")

intensidadeHora$activity_hour=as.POSIXct(intensidadeHora$activity_hour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensidadeHora$data <- format(intensidadeHora$activity_hour, format = "%d/%m/%Y")
intensidadeHora$hora <- format(intensidadeHora$activity_hour, format = "%H:%M:%S")

passosHora$activity_hour=as.POSIXct(passosHora$activity_hour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
passosHora$data <- format(passosHora$activity_hour, format = "%d/%m/%Y")
passosHora$hora <- format(passosHora$activity_hour, format = "%H:%M:%S")

sonoDia$sleep_day=as.POSIXct(sonoDia$sleep_day, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sonoDia$data <- format(sonoDia$sleep_day, format = "%d/%m/%Y")

#Inserindo a coluna dia_semana
atividadeDia <- atividadeDia %>% 
  mutate(dia_semana = weekdays(as.Date(data, "%d/%m/%Y")))

#ordenando os dias da semana
atividadeDia$dia_semana <- factor(atividadeDia$dia_semana, levels= c("segunda-feira", "terça-feira", 
                                                                     "quarta-feira", "quinta-feira", 
                                                                     "sexta-feira", "sábado", "domingo"))
caloriasHora <- caloriasHora %>% 
  mutate(dia_semana = weekdays(as.Date(data, "%d/%m/%Y")))
#ordenando os dias da semana
caloriasHora$dia_semana <- factor(caloriasHora$dia_semana, levels= c("segunda-feira", "terça-feira", 
                                                                     "quarta-feira", "quinta-feira", 
                                                                     "sexta-feira", "sábado", "domingo"))

#Resumo das medidas estatísticas
atividadeDia %>%
  select(total_steps,
         very_active_minutes,
         fairly_active_minutes,
         lightly_active_minutes,
         sedentary_minutes,
         calories) %>%
  summary()

caloriasHora %>%
  select(calories) %>%
  summary()

intensidadeHora %>%
  select(total_intensity,
         average_intensity) %>%
  summary()

passosHora %>%
  select(step_total) %>%
  summary()

sonoDia %>%
  select(total_minutes_asleep,
         total_time_in_bed) %>%
  summary()

#Unindo tabelas
diario_atividade_sono <- merge(atividadeDia, sonoDia, by=c("id","data"))
horario_calorias_intensidade_passos <- merge(caloriasHora, intensidadeHora, by=c("id","data","hora"))
horario_calorias_intensidade_passos <- merge(horario_calorias_intensidade_passos, passosHora, by=c("id","data","hora"))

#Porcentagem da Média do tempo consumido diariamente em cada nível de atividade
ativDia_media <- atividadeDia %>%
  summarise(mean(very_active_minutes), mean(fairly_active_minutes), 
            mean(lightly_active_minutes), mean(sedentary_minutes))

ativDia_mediaPorcent <- prop.table(ativDia_media)*100 #cálculo das porcentagens

ativPorcent <- data.frame(
  legenda=c("Muito ativo", "Razoavelmente ativo", "Levemente ativo", "Sedentário"),
  valores=c(ativDia_mediaPorcent$`mean(very_active_minutes)`,ativDia_mediaPorcent$`mean(fairly_active_minutes)`,
            ativDia_mediaPorcent$`mean(lightly_active_minutes)`,ativDia_mediaPorcent$`mean(sedentary_minutes)`))

#plotagem
plot_ly(ativPorcent, labels = ~legenda, values = ~valores, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  layout(title = 'Porcentagem da Média do tempo consumido diariamente em cada nível de atividade',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Relação entre o Total de Passos e o Gasto Calórico por dia
#plotagem
ggplot(data=atividadeDia, aes(x=calories, y=total_steps)) + 
  geom_point() + geom_smooth() +
  labs(title="Relação entre o Total de Passos e o Gasto Calórico por dia",x="Calorias gastas", y="Número de passos")

#Variação do Gasto Calórico (médio e máximo) durante a semana
#médio
caloriasDia_media <- atividadeDia %>% 
  group_by(dia_semana) %>% 
  summarise(calorias_media = mean(calories))

#plotagem
ggplot(data=caloriasDia_media, aes(x=dia_semana, y=calorias_media, 
                                   label=format(round(calorias_media, 1), nsmall=1)))+ 
  geom_bar(stat="identity", fill="steelblue")+
  geom_label()+
  theme(axis.text.x = element_text(angle = 15))+
  labs(title="Gasto Calórico Médio durante a semana", x="Dia da semana", y="Calorias gastas")

#máximo
caloriasDia_max <- atividadeDia %>% 
  group_by(dia_semana) %>% 
  summarise(calorias_max = max(calories))

#plotagem
ggplot(data=caloriasDia_max, aes(x=dia_semana, y=calorias_max,
                                 label=format(round(calorias_max, 1), nsmall=1)))+ 
  geom_bar(stat="identity", fill="steelblue")+
  geom_label()+
  theme(axis.text.x = element_text(angle = 15))+
  labs(title="Gasto Calórico Máximo durante a semana", x="Dia da semana", y="Calorias gastas")

#Variação da Média da Intensidade Total de acordo com o horário
intensidadeHora_media <- intensidadeHora %>%
  group_by(hora) %>%
  summarise(intensidade_total_media = mean(total_intensity))

#plotagem
ggplot(data=intensidadeHora_media, aes(x=hora, y=intensidade_total_media)) + 
  geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="Variação da Média da Intensidade Total de acordo com o horário", x="Horário", y="Média da intensidade total")

#Variação da Média de Calorias Gastas de acordo com o horário
caloriasHora_media <- caloriasHora %>% 
  group_by(hora) %>% 
  summarise(calorias_media = mean(calories))

#plotagem
ggplot(data=caloriasHora_media, aes(x=hora, y=calorias_media)) +
  geom_histogram(stat = "identity", fill= 'darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Variação da Média de Calorias Gastas de acordo com o horário", x="Horário", y="Média de calorias gastas")

#Variação do Tempo Médio em Estado Sedentário durante a semana
sedentarioDia_media <- atividadeDia %>% 
  group_by(dia_semana) %>% 
  summarise(sedentario_media = mean(sedentary_minutes))

#plotagem
ggplot(data=sedentarioDia_media, aes(x=dia_semana, y=sedentario_media,
                                     label=format(round(sedentario_media, 1), nsmall=1)))+ 
  geom_bar(stat="identity", fill="steelblue")+
  geom_label()+
  theme(axis.text.x = element_text(angle = 15))+
  labs(title="Variação do Tempo Médio em Estado Sedentário durante a semana", x="Dia da semana", y="Tempo em estado sedentário (min)")

#Relação entre Tempo Dormindo e Tempo em cada Nível de Atividade
#verificando valores máximo e mínimo para os limites do gráfico
summary(diario_atividade_sono)

#plotagem
ggplot(data=diario_atividade_sono) +
  
  geom_point(aes(x=total_minutes_asleep, y=very_active_minutes), color="blue") + 
  geom_smooth(aes(x=total_minutes_asleep, y=very_active_minutes), color="black", se=FALSE) +
  
  geom_point(aes(x=total_minutes_asleep, y=fairly_active_minutes), color="green") + 
  geom_smooth(aes(x=total_minutes_asleep, y=fairly_active_minutes), color="black", se=FALSE) +
  
  geom_point(aes(x=total_minutes_asleep, y=lightly_active_minutes), color="yellow") + 
  geom_smooth(aes(x=total_minutes_asleep, y=lightly_active_minutes), color="black", se=FALSE) +
  
  geom_point(aes(x=total_minutes_asleep, y=sedentary_minutes), color="red") + 
  geom_smooth(aes(x=total_minutes_asleep, y=sedentary_minutes), color="black", se=FALSE) +
  
  scale_x_continuous(limits = c(0,900))+
  scale_y_continuous(limits = c(0,1300))+
  
  annotate("text", x=760, y=65, label="Muito e Razoavelmente ativo", color="black", size=3)+
  annotate("text", x=805, y=210, label="Levemente ativo", color="black", size=3)+
  annotate("text", x=760, y=650, label="Sedentário", color="black", size=3)+
  
  labs(title="Relação entre Tempo Dormindo e Tempo em cada Nível de Atividade", x="Tempo dormindo (min)", y="Tempo de atividade (min)")

#Variação do Tempo Médio Dormindo durante a semana
sonoDia_media <- diario_atividade_sono %>% 
  group_by(dia_semana) %>% 
  summarise(sono_media = mean(total_minutes_asleep))

#plotagem
ggplot(data=sonoDia_media, aes(x=dia_semana, y=sono_media,
                               label=format(round(sono_media, 1), nsmall=1)))+ 
  geom_bar(stat="identity", fill="steelblue")+
  geom_label()+
  theme(axis.text.x = element_text(angle = 15))+
  labs(title="Variação do Tempo Médio Dormindo durante a semana", x="Dia da semana", y="Tempo médio dormindo (min)")
