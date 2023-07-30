# ESTUDO DE CASO: Como uma empresa de tecnologia de bem-estar pode agir com inteligência?
#### Autor: Guilherme Oliveira da Rocha Cunha

#### Data da última atualização: 28/07/2023

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
- Abrangente (_**C**omprehensive_): Os dados incluem os resultados a cada minuto de atividade física, frequência cardíaca e monitoramento do sono. São abrangidas informações sobre atividades diárias, passos e frequência cardíaca que podem ser usadas para explorar os hábitos dos usuários. Esse conjunto de dados pode ter algumas limitações devido ao fato da amostra ser de apenas 30 usuários diferentes.
- Atual (_**C**urrent_): Apesar dos dados serem do ano de 2016, como se trata de um estudo de caso, os dados serão tratados como atuais.
- Citado (_**C**ited_): Os dados foram coletados a partir de terceiros, portanto desconhecidos. Licença de domínio público.

**Observação:** Esse conjunto de dados pode ter algumas limitações devido ao fato da amostra ser de apenas 30 usuários diferentes e de não apresentar informações demográficas. Um viés de amostragem pode ser identificado, logo é aconselhado adicionar outros dados para ajudar a lidar com essas limitações.

## 3. Processamento

Inicialmente todas as planilhas foram nomeadas de acordo com as convenções de nomenclatura de arquivo, incluindo a data na nomenclatura das planilhas no formato padrão internacional aaaammdd.

### 3.1. Instalando os pacotes e abrindo as bibliotecas
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

### 3.2. Importando os conjuntos de dados
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
  
### 3.3. Limpeza de dados
#### 3.3.1. Verificando a quantidade de usuários distintos
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

#### 3.3.2. Verificando a quantidade de valores N/A
```
sum(is.na(atividadeDia))
sum(is.na(caloriasHora))
sum(is.na(intensidadeHora))
sum(is.na(passosHora))
sum(is.na(sonoDia))
```
Nenhum dos conjuntos de dados apresentou valores N/A.

#### 3.3.3. Verificando e removendo duplicatas
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

#### 3.3.4. Renomenado as colunas
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

### 3.4. Correção e consistência das colunas de data e hora
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

### 3.5. Inserindo a coluna _dia_semana_
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
### 4.1. Resumo das medidas estatísticas
```
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
```
Algumas descobertas interessantes deste resumo estatístico:
- A média de passos por dia é de cerca de 7600 passos e o valor máximo de mais de 36000 passos, sendo que 75% dos usuários dão menos que 10700 passos por dia.
- A média da quantidade de tempo em estado muito ativo por dia é de cerca de 21 minutos, já a média em estado sedentário é de 990 minutos.
- A média de calorias gasta por dia é de cerca de 2300 kcal e por hora de 97 kcal, o que faz sentido (97x24≈2300).
- A média de tempo dormindo por dia é de cerca de 420 minutos, ou 7 horas.

### 4.2. Unindo tabelas
Esta etapa foi realizada para que alguns conjuntos de dados fossem unidos (inner join) possibilitando análises utilizando suas variáveis.
```
diario_atividade_sono <- merge(atividadeDia, sonoDia, by=c("id","data"))
horario_calorias_intensidade_passos <- merge(caloriasHora, intensidadeHora, by=c("id","data","hora"))
horario_calorias_intensidade_passos <- merge(horario_calorias_intensidade_passos, passosHora, by=c("id","data","hora"))
```
**Observação:** O inner join funciona como uma interseção entre duas tabelas, retorna os valores em comum de ambas as tabelas, de acordo com uma condição dada. Como o conjunto de dados _sonoDia_ possui menos observações que o conjunto _atividadeDia_ logo o conjunto gerado _diario_atividade_sono_ terá menos observações que _atividadeDia_. Quando não for necessário utilizar esses dois conjuntos de dados gerados, serão utilizados os conjuntos processados anteriormente.

### 4.3. Porcentagem da Média do tempo consumido diariamente em cada nível de atividade
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
  layout(title = 'Porcentagem da Média do tempo consumido diariamente em cada nível de atividade',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
```

### 4.4. Relação entre o Total de Passos e o Gasto Calórico por dia
```
#plotagem
ggplot(data=atividadeDia, aes(x=calories, y=total_steps)) + 
  geom_point() + geom_smooth() +
  labs(title="Relação entre o Total de Passos e o Gasto Calórico por dia",x="Calorias gastas", y="Número de passos")
```

### 4.5. Variação do Gasto Calórico (médio e máximo) durante a semana
```
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
```

### 4.6. Variação da Média da Intensidade Total de acordo com o horário
```
intensidadeHora_media <- intensidadeHora %>%
  group_by(hora) %>%
  summarise(intensidade_total_media = mean(total_intensity))

#plotagem
ggplot(data=intensidadeHora_media, aes(x=hora, y=intensidade_total_media)) + 
  geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="Variação da Média da Intensidade Total de acordo com o horário", x="Horário", y="Média da intensidade total")
```

### 4.7. Variação da Média de Calorias Gastas de acordo com o horário
```
caloriasHora_media <- caloriasHora %>% 
  group_by(hora) %>% 
  summarise(calorias_media = mean(calories))

#plotagem
ggplot(data=caloriasHora_media, aes(x=hora, y=calorias_media)) +
  geom_histogram(stat = "identity", fill= 'darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Variação da Média de Calorias Gastas de acordo com o horário", x="Horário", y="Média de calorias gastas")
```

### 4.8. Variação do Tempo Médio em Estado Sedentário durante a semana 
```
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
```

### 4.9. Relação entre Tempo Dormindo e Tempo em cada Nível de Atividade
```
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
```

### 4.10. Variação do Tempo Médio Dormindo durante a semana
```
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
```

## 5. Compartilhamento
### 5.1.
![image](https://github.com/Gui-lherme-Oliv/Google_Data_Analytics-Estudo_de_caso_Bellabeat/assets/123426025/8ae6e84d-b6bc-442c-8a65-41e0c97f9229)

A partir do gráfico acima pode-se identificar que os usuários passam em média mais de 80% do tempo diário em estado sedentário. A soma do tempo médio dos outros tipos corresponde a menos de 20% do tempo.

**Recomendação:** O aplicativo da Bellabeat pode apresentar logo pela manhã um gráfico que mostra a divisão do tempo consumido em cada nível de atividade no dia anterior, para que o usuário tenha conhecimento e possa planejar seu dia de forma com que o tempo em atividade sedentária seja reduzido.

### 5.2.
![image](https://github.com/Gui-lherme-Oliv/Google_Data_Analytics-Estudo_de_caso_Bellabeat/assets/123426025/a6b2716e-09ad-44a4-b3c2-bb810ae364b2)

A partir do gráfico acima pode-se identificar que uma correlação positiva entre o total de passos dados diariamente e o gasto calórico, ou seja, quanto mais passos mais calorias são consumidas.

**Recomendação:** O aplicativo da Bellabeat pode oferecer a opção de definição de metas de passos diários e/ou calorias gastas e mostrar uma notificação quando atingi-la.

### 5.3.
![image](https://github.com/Gui-lherme-Oliv/Google_Data_Analytics-Estudo_de_caso_Bellabeat/assets/123426025/9a9b2cda-489b-47c0-9df6-b9a6b5596181)

A partir do gráfico acima pode-se identificar que os maiores gastos calóricos médio acontecem na terça-feira e no sábado e os menores acontecem na quinta-feira e no domingo. Provavelmente ocorre mais gasto aos sábados devido ao fato de geralmente as pessoas terem mais tempo livre e praticarem mais atividades físicas, e menos gasto ao domingo por utilizarem o dia para relaxar. De qualquer forma seria interessante obter mais dados para se ter maior precisão na análise, até pelo fato do comportamento dos dados nos outros dias da semana fugirem do senso comum, como exemplo do que ocorre na terça-feira e na quinta-feira.

### 5.4.
![image](https://github.com/Gui-lherme-Oliv/Google_Data_Analytics-Estudo_de_caso_Bellabeat/assets/123426025/da01ff3c-a41f-466b-8e87-4dee7667fa0f)

A partir do gráfico acima pode-se identificar que o maior valor registrado de gasto calórico aconteceu na quinta-feira e o menor valor aconteceu na sexta-feira.

**Recomendação:** O aplicativo da Bellabeat pode apresentar um resumo diário e/ou semanal dos valores de gastos calóricos.

### 5.5.
![image](https://github.com/Gui-lherme-Oliv/Google_Data_Analytics-Estudo_de_caso_Bellabeat/assets/123426025/33e995ee-ef7f-42ef-a3f3-011729a16238)

A partir do gráfico acima pode-se identificar que os usuários são mais ativos entre 05h e 22h, onde os maiores valores da intensidade toal média ocorrem entre 17:00h e 19:00h e os menores entre 02:00h e 04:00h. Provavelmente o pico ocorre entre 17:00h e 19:00h por conta de alguma atividade após o trabalho, como fazer uma caminhada ou ir em uma academia.

**Recomendação:** O aplicativo da Bellabeat pode apresentar um pouco antes desse horário uma notificação e/ou lembrete de motivação da prática de alguma atividade física.

### 5.6.
![image](https://github.com/Gui-lherme-Oliv/Google_Data_Analytics-Estudo_de_caso_Bellabeat/assets/123426025/66e61d17-e405-457a-993e-59a4b038c784)

A partir do gráfico acima pode-se identificar que os maiores gastos calóricos médio ocorrem entre 17:00h e 19:00h e os menores entre 02:00h e 04:00h. Como esperado, existe uma relação entre gasto calórico e intensidade total, ambos histogramas apresentam um comportamento semelhante.

**Recomendação:** O aplicativo da Bellabeat pode apresentar um pouco antes das 17h uma notificação e/ou lembrete de motivação da prática de alguma atividade física.

### 5.7.
![image](https://github.com/Gui-lherme-Oliv/Google_Data_Analytics-Estudo_de_caso_Bellabeat/assets/123426025/52f0c7d9-6922-44b1-94a6-840cf70a5f83)

A partir do gráfico acima pode-se identificar que os maiores tempos médio em estado sedentário ocorrem na segunda-feira e terça-feira e os menores na quinta-feira e no sábado. Como mencionado anteriormente, aos sábados geralmente as pessoas têm mais tempo livre e praticarem mais atividades físicas, mas seria considerável obter mais dados para se ter uma maior precisão da análise.

### 5.8.
![image](https://github.com/Gui-lherme-Oliv/Google_Data_Analytics-Estudo_de_caso_Bellabeat/assets/123426025/6b46d0f8-4ce3-4f0d-a0de-c50b59ec39bc)

A partir do gráfico acima pode-se identificar que existe uma correlação negativa entre o tempo dormindo e o tempo em atividade sedentária.

**Recomendação:** Se os usuários do aplicativo da Bellabeat quiserem melhorar o sono, o aplicativo pode recomendar a redução do tempo em estado sedentário.

### 5.9.
![image](https://github.com/Gui-lherme-Oliv/Google_Data_Analytics-Estudo_de_caso_Bellabeat/assets/123426025/fa949e57-6285-4f88-b98a-966bfc810763)

A partir do gráfico acima pode-se identificar que o maiores tempos médio dormindo ocorrem na quarta-feira e no domingo e os menores na terça-feira e na quinta-feira.

**Recomendação:** O aplicativo da Bellabeat pode oferecer a opção de definir notificações de acordo com o horário, para que tempo médio dormindo seja consistente durante toda a semana.

## 6. Ação
### 6.1. Considerações
- É importante destacar que o uso de mais dados é de extrema importância para melhorar a precisão das análises e para poder gerar mais insights.
- Lembrar que a correlação entre alguns dados não significa necessariamente causalidade.

### 6.2. Recomendações para a estratégia de marketing
- O aplicativo da Bellabeat pode apresentar aos seus usuários estudos que evidenciem quais são os valores recomendados para se ter uma vida saudável, como o número mínimo de passos totais por dia, máximo de tempo em atividade sedentária, tempo mínimo dormindo, entre outros.
- A Bellabeat pode oferecer um sistema de recompensa e ganho de pontos, onde incentiva os usuários a praticarem atividades físicas e onde eles ganham mais pontos nos dias que, de acordo com as análises, apresentam menores valores de intensidade total e/ou mais tempo em estado sedentário. Podem ser definidas diferentes metas, de acordo com o total de passos, calorias gastas ou alguma outra métrica, e os usuários que cumprirem as metas recebem descontos em planos de assinatura ou em produtos da Bellabeat.
- A Bellabeat pode fechar parceria com marcas com foco em estilo de vida saudável, como alimentos e academias, onde os pontos do sistema de recompensa possam ser utilizados para descontos em seus produtos e serviços.
- O produtos Leaf e Time podem vibrar após identificarem um período prolongado em estado sedentários, além de também poderem sinalizar que é hora de dormir depois de identificar um tempo prolongado acordado na cama.








