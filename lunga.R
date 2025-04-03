---
  title: |
  \begin{center}
{\Large \textbf{ALMA MATER STUDIORUM – UNIVERSITA’ DI BOLOGNA}}\\[1cm]
{\large DIPARTIMENTO DI SCIENZE STATISTICHE}\\
{\large “PAOLO FORTUNATI”}\\[1cm]
{\large Corso di Laurea in Scienze Statistiche}\\[2cm]
\end{center}
\begin{center}
{\Large \textbf{DETERMINANTI DELLA SPERANZA DI VITA:UN'ANALISI GLOBALE DEGLI EFFETTI DI INQUINAMENTO, ABITUDINI DI CONSUMO E CONDIZIONI SANITARIE}}\\[0.5cm]
  {\Large\textit{ Utilizzo Statistico di Banche Dati Online}}
  \end{center}

author: "Lucrezia Galli"
date: "2025-04-06"
format: 
  pdf:
    number-sections: true  # Enables section numbering
bibliography: biblio.bib  # Link the BibTeX file
header-includes: |
  % Set Arial font globally
  \usepackage{helvet}  
  \renewcommand{\familydefault}{\sfdefault}  


  % Ensure correct document structure
  \usepackage{titling}
  \usepackage{array}
  \usepackage{ragged2e}

  % Define Presentata and Appello sections
  \newcommand{\presentata}{%
    \begin{flushleft}
    \begin{tabular}{ p{7cm} p{7cm} }
    \textbf{Presentata da:} & \hfill \textbf{Relatore:} \\
    Lucrezia Galli & \hfill Prof Paolo Verme \\
    Matricola: 0001113963 &  \\
    \end{tabular}
    \end{flushleft}
  }
  \newcommand{\appello}{%
    \vspace{1cm}
    \begin{center}
    \textbf{APPELLO I}\\[0.5cm]
    \textbf{ANNO ACCADEMICO 24 / 25}
    \end{center}
  }
---



```{r, include=F}
source("C:/Users/luca galli/OneDrive/Documenti/lucrezia secondo anno/utilizzo banche dati/pacchetti.R")
```
 

```{r setup, include=FALSE}
#| output: false
source("C:/Users/luca galli/OneDrive/Documenti/lucrezia secondo anno/utilizzo banche dati/pacchetti.R")
ifpack(c("readxl", "dplyr", "knitr", "kableExtra", "ggplot2", "viridis", "car","tidyverse", "sf", "countrycode", "rnaturalearth", "rnaturalearthdata"))

```


\presentata

\appello

\newpage  

\fontsize{12pt}{14pt} \selectfont  

# Introduction

Analizzare i fattori che influenzano la speranza di vita è essenziale per sviluppare strategie efficaci di sanità pubblica e promuovere il benessere a livello globale. La letteratura esistente ha messo in evidenza il ruolo di variabili socioeconomiche, dell’accesso alle cure mediche e delle abitudini di vita nella determinazione della longevità, ma una visione integrata che consideri contemporaneamente diversi fattori rimane ancora poco esplorata. Questo studio si propone di esaminare l’impatto dell’inquinamento atmosferico, del consumo di alcol e tabacco, della mortalità materna, degli incidenti stradali e delle malattie croniche sulla speranza di vita, fornendo un’analisi completa e approfondita.

# Data
Il dataset utilizzato per l'analisi non è stato scaricato direttamente da una singola fonte, ma costruito a partire da diversi dataset disponibili nei database della [World Health Organization (WHO)](https://apps.who.int/gho/data/node.main.1?lang=en).  Ogni dataset conteneva una singola variabile osservata per più anni; per garantire coerenza temporale, sono stati selezionati i dati relativi all'anno 2019. Successivamente, le variabili di interesse sono state aggregate in un unico file Excel, creando così una nuova tabella strutturata. Questa è stata poi importata in R per l'analisi statistica.
  
  ```{r, include=FALSE}
  who_data <- read_excel("whohealth.xlsx")
  who_data<-who_data[,c(-2)]
  
  #da character a numeric
  who_data <- who_data %>%
    mutate(across(2:ncol(who_data), ~as.numeric(gsub("[^0-9.-]", "", .))))
  
  str(who_data)
  summary(who_data)
  colMeans(who_data[,-1])
  colnames(who_data)[2]<-'LifeExpatBirth'
  colnames(who_data)[6]<-'Alcohol_Consumption'
  colnames(who_data)[7]<-'Pop_Basic_Drinking_Water'
  ```
  
  ```{r, echo=FALSE}
  tabella_info <- data.frame(
    Variabile = c("LifeExpatBirth", "Mortality_30_70_CCDR", "PM25_Annual_Urban", 
                  "Maternal_Mortality_Ratio", "Alcohol_Consumption", "Tobacco_Age_Stand",
                  "Pop_Basic_Drinking_Water", "Road_traffic_injuries"),
    Descrizione = c("Speranza di vita alla nascita", 
                    "Probabilità di morire tra i 30 e i 70 anni a causa di malattie croniche","Concentrazione media annua di particolato fine (PM2.5) nell’aria delle aree urbane", 
                    "Numero di decessi materni per complicazioni legate alla gravidanza o al parto", "Quantità media annua di alcol consumata dalla popolazione di età superiore ai 15 anni","Percentuale della popolazione che fa uso di prodotti a base di tabacco","Percentuale della popolazione con accesso a servizi idrici di base", "Tasso di mortalità dovuto a incidenti stradali"),
    Unita_di_misura = c("Anni", "Tasso (%)", "µg/m³", 
                        "Decessi per 100.000 nati vivi", "Litri pro capite", 
                        "Percentuale (%)", "Percentuale (%)",   "Tasso per 100.000 abitanti")
  )
  
  
  kable(tabella_info, caption = "Descrizione delle variabili chiave del dataset", 
        align = "lcc", booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    column_spec(2, width = "7cm") %>%
    column_spec(3, width = "3cm")
  
  
  ```
  # Methodology
  Per esaminare i fattori che influenzano l'aspettativa di vita alla nascita, è stato adottato un modello di regressione lineare multipla. Questo metodo permette di stimare l'effetto di più variabili esplicative sulla variabile dipendente, mantenendo costanti gli altri fattori. Il modello include variabili legate alla salute pubblica, all'ambiente e agli stili di vita.
Il modello di regressione è il seguente:
\
\resizebox{\textwidth}{!}{$
LifeExp = \beta_0 + \beta_1 \cdot Mort3070 + \beta_2 \cdot PM25 + 
\beta_3 \cdot MatMort + \beta_4 \cdot AlcCons + 
\beta_5 \cdot DrinkWater + \beta_6 \cdot RoadInj + 
\beta_7 \cdot TobUse + \epsilon
$}
\
dove *LifeExpatBirth* rappresenta l'aspettativa di vita alla nascita e le altre variabili sono i fattori esplicativi. Il termine *ϵ* indica l'errore residuo del modello. La bontà del modello è stata valutata attraverso il coefficiente di determinazione (R²), mentre il Variance Inflation Factor (VIF) è stato calcolato per individuare eventuali problemi di multicollinearità. Inoltre, è stato realizzato un grafico dei residui rispetto ai valori predetti per verificare la presenza di eventuali schemi non casuali e valutare l'adeguatezza del modello.
  
  # Results
  
  ## Effetti delle variabili sulla speranza di vita
  ```{r, echo=FALSE}
  lmod<-lm(LifeExpatBirth~ Mortality_30_70_CCDR+PM25_Annual_Urban+Maternal_Mortality_Ratio+Alcohol_Consumption+Pop_Basic_Drinking_Water+Road_traffic_injuries+Tobacco_Age_Stand, data=who_data)
  
  
  results_table <- as.data.frame(summary(lmod)$coefficients)
  
  results_table <- tibble::rownames_to_column(results_table, var = "Variable")
  
  
  kable(results_table, caption = "Risultati del Modello Multiplo", digits = 3)
  
  ```
  
  ```{r}
  summary(lmod)$r.squared
  ```
  
  
  ```{r}
  vif(lmod)
  ```
  
  Il modello di regressione multipla ha un R² di 0.9198, indicando che il 91.98% della variabilità della speranza di vita è spiegata dalle variabili incluse. La mortalità tra i 30 e i 70 anni (-0.512, p < 0.001), il tasso di mortalità materna (-0.007, p < 0.001), l'inquinamento atmosferico urbano (PM2.5) (-0.027, p = 0.032) e le morti per incidenti stradali (-0.124, p < 0.001) hanno un impatto negativo significativo sulla speranza di vita. Al contrario, l'accesso a servizi idrici di base (+0.104, p < 0.001) è associato a un aumento della speranza di vita. Il consumo di tabacco (+0.054, p = 0.004) mostra un effetto positivo inaspettato, mentre il consumo di alcol (-0.031, p = 0.519) non è significativo.
  
  Nonostante l'R² elevato, i VIF (tutti inferiori a 5) indicano l'assenza di multicollinearità tra le variabili indipendenti, confermando la stabilità delle stime del modello. Complessivamente, i risultati evidenziano il forte impatto delle condizioni sanitarie e ambientali sulla longevità.
  
  
  ## Relazione tra mortalità prematura e speranza di vita
  Dopo aver stimato un modello lineare per analizzare i fattori che influenzano la speranza di vita alla nascita, ho individuato Mortality_30_70_CCDR (la probabilità di morire tra i 30 e i 70 anni per cause croniche) come la variabile più significativa. Questo risultato è stato determinato osservando i p-value dei coefficienti del modello, escludendo l’intercetta, e identificando la variabile con il p-value più basso.
  Per comprendere meglio la relazione tra Mortality_30_70_CCDR e LifeExpatBirth, ho realizzato un grafico a dispersione con una retta di regressione. 
  ```{r, echo=FALSE}
  coeff_table <- summary(lmod)$coefficients[-1,]
  most_significant <- which.min(coeff_table[, 4])  
  coeff_table[most_significant, , drop = FALSE]  
  ggplot(who_data, aes(x = Mortality_30_70_CCDR, y = LifeExpatBirth)) +
    geom_point(alpha = 0.6, color = "blue") +  # Punti di dispersione
    geom_smooth(method = "lm", color = "red") +    # Retta di regressione
    labs(title = "Relazione tra Mortalità (30-70 anni) e Speranza di Vita",
         x = "Probabilità di morte (30-70 anni)",
         y = "Speranza di vita (anni)") +
    theme_minimal()
  
  
  ```
  Il grafico mostra chiaramente una relazione negativa: all'aumentare della probabilità di morte tra i 30 e i 70 anni, la speranza di vita alla nascita diminuisce. Questo è coerente con l’aspettativa che una maggiore mortalità in questa fascia d’età riduca la durata media della vita.

La retta di regressione (in rosso) conferma questa tendenza negativa, mentre l'area grigia rappresenta l'intervallo di confidenza, indicando l’incertezza nelle stime. La distribuzione dei punti suggerisce una relazione piuttosto forte tra le due variabili, confermando l'importanza di Mortality_30_70_CCDR come predittore chiave della speranza di vita.
  
  ## Distribuzione della speranza di vita per continente
  
  Per esplorare le differenze nella speranza di vita alla nascita tra i vari continenti, è stata realizzata una mappa tematica. Questa visualizzazione consente di apprezzare chiaramente le disparità tra le diverse aree geografiche, offrendo una rappresentazione immediata dei valori e facilitando il confronto tra le regioni del mondo.
  ```{r, echo=FALSE}
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  who_data <- who_data %>% mutate(iso3=countrycode(Country, origin = "country.name",destination = "iso3c"))
  
  world_data <- left_join(world, who_data, by = c("iso_a3" = "iso3"))
  
  ggplot(world_data) +
    geom_sf(aes(fill =LifeExpatBirth))+ 
    scale_fill_viridis_c(option = "plasma", na.value='white') +
    labs(title = "Speranza di vita alla nascita nel mondo",
         fill = "Anni") +
    theme_minimal()
  
  ```
  
  La mappa offre una rappresentazione visiva immediata delle disuguaglianze nella speranza di vita alla nascita tra i continenti. Le aree con valori più bassi, come l'Africa subsahariana, sono evidenziate con tonalità di viola scuro, mentre le regioni con speranze di vita più alte, come Nord America, Europa e Australia, sono contrassegnate da colori più caldi come il giallo. Questo contrasto cromatico rende facile identificare le disparità globali, facilitando l'interpretazione delle differenze regionali nella speranza di vita.
  
  
  ## Visualizzazione dei risultati
  Il grafico "Residui vs. Valori Predetti" è uno strumento fondamentale per valutare l'adeguatezza del modello di regressione lineare. Nel grafico, i residui sono rappresentati rispetto ai valori predetti dal modello, consentendo di osservare eventuali schemi sistematici o irregolarità.
```{r, echo=FALSE}
plot(lmod$fitted.values, resid(lmod),
     xlab = "Valori Predetti",
     ylab = "Residui",
     main = "Figura 1: Residui vs. Valori Predetti",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)
```
Osservando il grafico, si nota una dispersione abbastanza casuale dei residui intorno alla linea orizzontale a zero, senza evidenti pattern sistematici. Questo è un buon segnale, poiché indica che l'ipotesi di omoschedasticità è plausibile e che il modello non lascia sistematicamente fuori delle informazioni rilevanti.
  
  Tuttavia, si può osservare una leggera maggiore concentrazione dei residui positivi per valori predetti più elevati, il che potrebbe suggerire una possibile sottostima della speranza di vita nei casi con valori predetti più alti. Questo comportamento potrebbe indicare una lieve distorsione, ma nel complesso il modello appare adeguato.
  
  # Conclusions
  Questo studio ha esaminato in modo integrato l'impatto di fattori ambientali, sanitari e comportamentali sulla speranza di vita, colmando una lacuna nella letteratura. I risultati mostrano che la mortalità prematura, l'inquinamento atmosferico e gli incidenti stradali hanno un effetto negativo significativo, mentre l’accesso ai servizi idrici di base è associato a una maggiore longevità. Il modello adottato ha spiegato gran parte della variabilità osservata, confermando la necessità di strategie di sanità pubblica mirate per ridurre i principali fattori di rischio e migliorare il benessere globale.
  
  # References
  
  World Health Organization. (2025). *Global Health Observatory data: Health workforce statistics*. World Health Organization. Recuperato da [https://apps.who.int/gho/data/node.main?showonly=HWF](https://apps.who.int/gho/data/node.main?showonly=HWF)
  