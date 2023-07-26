# ESTUDO DE CASO: Como uma empresa de tecnologia de bem-estar pode agir com inteligência?
#### Autor: Guilherme Oliveira da Rocha Cunha

#### Data da última atualização: 26/07/2023

## Cenário
Você é um analista de dados júnior que trabalha na equipe de analistas de marketing da Bellabeat, uma fabricante de produtos de alta tecnologia voltados à saúde para mulheres. A Bellabeat é uma pequena empresa de sucesso, mas tem potencial para se adquirir maior participação no mercado global de dispositivos inteligentes. Urška Sršen, cofundadora e CEO da Bellabeat, acredita que a análise de dados de condicionamento físico a partir de dispositivos inteligentes pode ajudar a abrir novas oportunidades de crescimento para a empresa. Foi solicitado que você se concentre em um dos produtos da Bellabeat e analise dados de dispositivos inteligentes para obter informações sobre como os consumidores estão usando esses dispositivos. Os insights que você descobrir ajudarão a orientar a estratégia de marketing da empresa. Você apresentará sua análise à equipe executiva da Bellabeat juntamente com suas recomendações de alto nível para a estratégia de marketing da empresa.

## Personagens e Produtos
#### Personagens
- Urška Sršen: Cofundadora e CEO da Bellabeat
- Sando Mur: Matemático e cofundador da Bellabeat; membro-chave da equipe executiva da Bellabeat
- Equipe de análise de marketing da Bellabeat: Uma equipe de analistas de dados responsável por coletar, analisar e relatar dados que ajudam a orientar a estratégia de marketing da Bellabeat. Você se juntou a esta equipe há seis meses e tem estado ocupado aprendendo sobre a missão e os objetivos de negócios da Bellabeat – e como você, como analista de dados júnior, também pode ajudar a Bellabeat a alcançá-los.

#### Produtos
- Aplicativo Bellabeat: O aplicativo Bellabeat fornece aos usuários dados de saúde relacionados à sua atividade, sono, estresse, ciclo menstrual e hábitos de atenção plena. Esses dados podem ajudar os usuários a entender melhor seus hábitos atuais e tomar decisões saudáveis. O aplicativo Bellabeat se conecta à sua linha de produtos inteligentes de bem-estar.
- Leaf: O rastreador de bem-estar clássico da Bellabeat pode ser usado como pulseira, colar ou clipe. O rastreador Leaf se conecta ao aplicativo Bellabeat para rastrear a atividade, o sono e o estresse.
- Time: Este relógio de bem-estar combina a aparência atemporal de um relógio clássico com tecnologia inteligente para rastrear a atividade, o sono e o estresse do usuário. O relógio Time se conecta ao aplicativo Bellabeat para fornecer informações sobre seu bem-estar diário.
- Spring: Esta é uma garrafa de água que rastreia a ingestão diária de água por meio de tecnologia inteligente para garantir que você esteja adequadamente hidratado ao longo do dia. A garrafa Spring se conecta ao aplicativo Bellabeat para rastrear seus níveis de hidratação.
- Planos da Bellabeat: A Bellabeat também oferece aos usuários diferentes planos de assinatura. As assinaturas oferecem aos usuários acesso 24 horas por dia, 7 dias por semana, orientação totalmente personalizada sobre nutrição, atividade, sono, saúde e beleza, além de atenção plena com base em seu estilo de vida e objetivos.

## Sobre a empresa
Urška Sršen e Sando Mur fundaram a Bellabeat, uma empresa de alta tecnologia que fabrica produtos inteligentes focados na saúde. A Urška aproveitou sua experiência como artista para desenvolver uma tecnologia elegantemente projetada que informa e inspira mulheres em todo o mundo. A coleta de dados sobre atividade, sono, estresse e saúde reprodutiva permitiu à Bellabeat capacitar as mulheres com conhecimento sobre sua própria saúde e hábitos. Desde que foi fundada em 2013, a Bellabeat cresceu rapidamente e não levou muito tempo para se posicionar como uma empresa de bem-estar voltada à tecnologia para mulheres.

Em 2016, a Bellabeat abriu escritórios ao redor do mundo e lançou vários produtos. Os produtos Bellabeat tornaram-se disponíveis por meio de um número crescente de varejistas online, além de seu próprio canal de comércio eletrônico em seu site. A empresa investiu em mídia de publicidade tradicional, como rádio, outdoors, mídia impressa e televisão, mas se concentra amplamente no marketing digital. A Bellabeat investe o ano todo na Pesquisa do Google, mantendo páginas ativas no Facebook e Instagram, além de engajar os consumidores de forma consistente no Twitter. Além disso, a Bellabeat exibe anúncios em vídeo no Youtube e anúncios gráficos na rede de display do Google para apoiar campanhas em datas importantes de marketing.

A Urška sabe que uma análise dos dados de consumo disponíveis da Bellabeat revelaria mais oportunidades de crescimento. Ela pediu à equipe de análise de marketing para se concentrar em um produto da Bellabeat e analisar os dados de uso de dispositivos inteligentes para obter informações sobre como as pessoas já estão usando seus dispositivos inteligentes. Assim, por meio dessas informações, ela gostaria de conferir excelentes recomendações sobre como essas tendências podem nortear a estratégia de marketing da Bellabeat.

## Etapas do processo de análise de dados

### ❔ [Pergunta](#1-pergunta)
### 💻 [Preparação](#2-preparação)
### 🛠 [Processamento](#3-processamento)
### 📊 [Análise](#4-análise)
### 📋 [Compartilhamento](#5-compartilhamento)
### 🧗 [Ação](#6-ação)

## 1. Pergunta
#### Tarefa de negócios: Analisar os dados de uso de dispositivos inteligentes para obter informações sobre como os consumidores usam dispositivos inteligentes que não são da Bellabeat. Em seguida, gerar insights visando ajudar a orientar a estratégia de marketing da empresa para que ela adquira maior participação no mercado global de dispositivos inteligentes.

Partes interessadas primárias: Urška Sršen e Sando Mur, membros do time executivo.

Partes interessadas secundárias: Equipe de análise de marketing da Bellabeat.

## 2. Preparação
O conjunto de dados utilizado foi o _FitBit Fitness Tracker Data_ ou, em português, Dados do rastreador de condicionamento físico FitBit (CC0: Domínio público, conjunto de dados disponibilizado por meio de Möbius): Este [conjunto de dados do Kaggle](https://www.kaggle.com/datasets/arashnic/fitbit) contém os dados obtidos entre 12/03/2016 e 12/05/2016 dos rastreadores de condicionamento físico pessoal de trinta usuários do Fitbit. Trinta usuários elegíveis do FitBit consentiram com o envio de dados pessoais do rastreador, incluindo os resultados a cada minuto de atividade física, frequência cardíaca e monitoramento do sono. São abrangidas informações sobre atividades diárias, passos e frequência cardíaca que podem ser usadas para explorar os hábitos dos usuários. 

O conjunto de dados contém aproximadamente 322 MB, disposto em 18 planilhas no formato CSV (_Comma-separated values_) com os dados organizados no formato longo (cada usuário terá dados em várias linhas).

Uma boa fonte de dados deve seguir a abordagem **ROCCC**:
- Confiável (_**R**eliable_): Os dados consentidos pelos usuários do FitBit foram gerados através de uma pesquisa distribuída mediante o serviço de crowdsourcing Amazon Mechanical Turk.
- Original (_**O**riginal_): Trinta usuários elegíveis do FitBit consentiram com o envio de dados pessoais do rastreador.
- Compreensivo (_**C**omprehensive_): Os dados incluem os resultados a cada minuto de atividade física, frequência cardíaca e monitoramento do sono. São abrangidas informações sobre atividades diárias, passos e frequência cardíaca que podem ser usadas para explorar os hábitos dos usuários. Esse conjunto de dados pode ter algumas limitações devido ao fato da amostra ser de apenas 30 usuários diferentes.
- Atual (_**C**urrent_): Apesar dos dados serem do ano de 2016, como se trata de um estudo de caso, os dados serão tratados como atuais.
- Citado (_**C**ited_): Os dados foram coletados a partir de terceiros, portanto desconhecidos. Licença de domínio público.

**Observação:** Esse conjunto de dados pode ter algumas limitações devido ao fato da amostra ser de apenas 30 usuários diferentes e de não apresentar informações demográficas. Um viés de amostragem pode ser identificado, logo é aconselhado adicionar outros dados para ajudar a lidar com essas limitações.

## 3. Processamento

Inicialmente todas as planilhas foram nomeadas de acordo com as convenções de nomenclatura de arquivo, incluindo a data na nomenclatura das planilhas no formato padrão internacional aaaammdd.

### 3.1 Instalando os pacotes e abrindo as bibliotecas
```
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
```

### 3.2 Importando os conjuntos de dados
```
atividadeDia <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged_20160312-20160512.csv")
caloriasHora <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged_20160312-20160512.csv")
intensidadeHora <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged_20160312-20160512.csv")
passosHora <- read.csv ("~/FitBit/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged_20160312-20160512.csv")
peso <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged_20160312-20160512.csv")
sonoDia <- read.csv("~/FitBit/Fitabase Data 4.12.16-5.12.16/sleepDay_merged_20160312-20160512.csv")
```
Sobre os conjuntos de dados que serão utilizados para a análise:
- _atividadeDia_: Apresenta os valores por dia das variáveis TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories entre outras;
- _caloriasHora_: Apresenta os valores por dia e hora da variável Calories;
- _intensidadeHora_: Apresenta os valores por dia e hora das variáveis TotalIntensity e AverageIntensity;
- _passosHora_: Apresenta os valores por dia e hora da variável StepTotal;
- _peso_: Apresenta os valores da variável WeightKg entre outras;
- _sonoDia_: Apresenta os valores por dia das variáveis TotalMinutesAsleep, TotalTimeInBed entre outras.

Visualizando e consultando o resumo das medidas estatísticas dos conjuntos de dados:
```
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
```
  
### 3.3 Limpeza de dados
#### 3.3.1 Verificando a quantidade de usuários distintos
```
n_distinct(atividadeDia$Id)
n_distinct(caloriasHora$Id)
n_distinct(intensidadeHora$Id)
n_distinct(passosHora$Id)
n_distinct(sonoDia$Id)
n_distinct(peso$Id)
```

Valores apresentados:
- 33 para atividadeDia
- 33 para caloriasHora
- 33 para intensidadeHora
- 33 para passosHora
- 24 para sonoDia
- 8 para peso

Por ser uma amostra de dados muito pequena, o conjunto de dados _peso_ não será utilizado na análise.

#### 3.3.2 Verificando a quantidade de valores N/A
```
sum(is.na(atividadeDia))
sum(is.na(caloriasHora))
sum(is.na(intensidadeHora))
sum(is.na(passosHora))
sum(is.na(sonoDia))
```
Nenhum dos conjuntos de dados apresentou valores N/A.

#### 3.3.3 Verificando e removendo duplicatas
```
sum(duplicated(atividadeDia))
sum(duplicated(caloriasHora))
sum(duplicated(intensidadeHora))
sum(duplicated(passosHora))
sum(duplicated(sonoDia))
```
Apenas o conjunto de dados _sonoDia_ apresentou duplicatas. Removendo-as:
```
sonoDia <- sonoDia %>%
  distinct()
```

#### 3.3.4 Renomenado as colunas
Para garantir que o nome das colunas estejam utilizando a sintaxe correta, sejam únicos e consistentes, as colunas serão renomeadas utilizando a função _clean_names()_.
```
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
```

### 3.4 Correção e consistência das colunas de data e hora
Esta etapa foi realizada para que a partir das colunas referentes à data em cada um dos conjuntos de dados, seja criada uma coluna nomeada _data_ no formato dd/mm/aaaa. A partir das colunas referentes à data e hora foram criadas duas colunas nomeadas _data_ e _hora_ no formato dd/mm/aaaa e hh:mm:ss, respectivamente.

```
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
```

### 3.5 Inserindo a coluna _dia_semana_
A coluna _dia_semana_ representa o dia da semana referente à cada uma das datas da coluna _data_ criada anteriormente.
```
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
```

## 4. Análise
### 4.1 Unindo tabelas
Esta etapa foi realizada para que algumas tabelas fossem unidas possibilitando análises utilizando suas variáveis
```
diario_atividade_sono <- merge(atividadeDia, sonoDia, by=c("id","data"))
horario_calorias_intensidade_passos <- merge(caloriasHora, intensidadeHora, by=c("id","data","hora"))
horario_calorias_intensidade_passos <- merge(horario_calorias_intensidade_passos, passosHora, by=c("id","data","hora"))
```
### 4.2 Média do tempo consumido diariamente em cada nível de atividade
```
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
  layout(title = 'Média do tempo consumido diariamente em cada nível de atividade',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
```

### 4.3 Relação entre o Total de Passos e o Gasto Calórico por dia
```
#plotagem
ggplot(data=atividadeDia, aes(x=calories, y=total_steps)) + 
  geom_point() + geom_smooth() +
  labs(title="Relação entre o Total de Passos e o Gasto Calórico por dia",x="Calorias Gastas", y="Número de Passos")
```

### 4.4 Gasto Calórico durante a semana
```
#média
caloriasDia_media <- atividadeDia %>% 
  group_by(dia_semana) %>% 
  summarise(calorias_media = mean(calories))

#plotagem
ggplot(data=caloriasDia_media, aes(x=dia_semana, y=calorias_media))+ 
  geom_bar(stat="identity", fill="steelblue")+
  theme(axis.text.x = element_text(angle = 15))+
  labs(title="Gasto Calórico médio durante a semana", x="Dia da Semana", y="Calorias Gastas")

#máximo
caloriasDia_max <- atividadeDia %>% 
  group_by(dia_semana) %>% 
  summarise(calorias_max = max(calories))

#plotagem
ggplot(data=caloriasDia_max, aes(x=dia_semana, y=calorias_max))+ 
  geom_bar(stat="identity", fill="steelblue")+
  theme(axis.text.x = element_text(angle = 15))+
  labs(title="Gasto Calórico máximo durante a semana", x="Dia da Semana", y="Calorias Gastas")
```

### 4.5 Variação da Intensidade Total Média de acordo com o horário
```
intensidadeHora_media <- intensidadeHora %>%
  group_by(hora) %>%
  summarise(intensidade_total_media = mean(total_intensity))

#plotagem
ggplot(data=intensidadeHora_media, aes(x=hora, y=intensidade_total_media)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="Variação da Intensidade Total Média de acordo com o horário", x="Horário", y="Intensidade Total Média")
```








