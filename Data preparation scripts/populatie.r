###################################################################################
#Statistici populatie
###################################################################################
populatia_romaniei_per_localitati<-read_excel(
  path=paste(project_path,"Statistici_populatie","Populatia Romaniei.xls",sep="//"),
  sheet=4,
  skip=3,
  col_names=c("Localitati","Total","Masculin","Feminin"),
  col_types=c("text","numeric","numeric","numeric")
)
populatia_romaniei_per_localitati<-
  populatia_romaniei_per_localitati[!populatia_romaniei_per_localitati$"Localitati" %in% c(NA), ] #eliminare valori nule
populatia_romaniei_per_localitati<-populatia_romaniei_per_localitati %>% filter(grepl("(MUNICIPIUL)",Localitati)) #luam in considerare doar muncipiile
populatia_romaniei_per_localitati$Localitati<-gsub("(MUNICIPIUL\\s)|(MUNICIPIUL\\s\\s)","",populatia_romaniei_per_localitati$Localitati) #eliminam cuvantul 'MUNICIPIUL'
populatia_romaniei_per_localitati<-populatia_romaniei_per_localitati[order(populatia_romaniei_per_localitati$Total,
                                                                           decreasing = TRUE),
                                                                     ] #sortare descendenta dupa numarul total de locuitori
