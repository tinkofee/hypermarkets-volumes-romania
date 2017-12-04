###################################################################################
#Mapare pagini cu detalii despre hipermarketuri
###################################################################################
lista_lanturi_retail<-data.frame(
      nume_lant=c("Carrefour",
                  "Cora",
                  "Kaufland",
                  "Metro",
                  "Selgros",
                  "Auchan"),
   link_magazin=c("https://www.carrefour.ro/magazine/?tip=hipermarket",
                  "https://www.cora.ro/store-locator",
                  "https://www.kaufland.ro/utile/magazin.storeName=.html",
                  "https://www.metro.ro/magazinele-noastre",
                  "https://www.selgros.ro/magazine",
                  "http://www.auchan.ro/magazinul-tau/"
   )
)
###################################################################################
#WEB-scrapping area
###################################################################################
#1 - Verifica daca botii au acces permis sa faca scrapping
###################################################################################
paths_allowed(lista_lanturi_retail$link_magazin, warn = FALSE)
###################################################################################
#2 - Citirea paginilor cu detalii despre hipermarketuri
###################################################################################

data_dir<-"data_html" 
dir.create(data_dir,showWarnings = FALSE) #creare folder unde se stocheaza paginile descarcate


descarca_pagini<-function(urls){
   for(url in urls){
      download(as.character(url), 
               destfile = file.path(data_dir, 
                                    paste(
                                       substr(url,
                                              which(strsplit(url,"")[[1]]==".")[1]+1,
                                              which(strsplit(url,"")[[1]]==".")[2]-1
                                              ),
                                       "html",
                                       sep="."
                                    )
               )
      )
   }
}

#download fiecare pagina de detalii despre magazine
walk(lista_lanturi_retail$nume_lant, 
     descarca_pagini(lista_lanturi_retail$link_magazin)
     )
###################################################################################
#Details gathering from html files for each retailer
###################################################################################
###################################################################################
                                       #Auchan
###################################################################################
date_Auchan<-read_html("data_html/auchan.html")
date_Auchan1<-date_Auchan %>% 
   html_nodes("text//javascript") %>%
   map(xml_attrs) %>% 
   map_df(~as.list(.))


date_auchan2<-html_nodes(date_Auchan,"select")



















