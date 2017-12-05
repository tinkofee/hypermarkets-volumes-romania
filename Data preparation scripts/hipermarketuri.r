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
magazine_Auchan<-read_html("data_html/auchan.html") #Auchan HTML file contents
###################################################################################
#Auchan counties - Actually all the hypermarkets are present in the next JSON structure, so no need this one
#                 If you don't need counties, delete this part of code.
###################################################################################
#judete_Auchan<-regmatches(html_text(date_Auchan),
#           gregexpr("(?<=var countiesData = ).*?(?=];)", html_text(date_Auchan), perl = TRUE)
#           )
#judete_Auchan<-as.character(judete_Auchan)
#judete_Auchan<-paste(judete_Auchan,"]")
#judete_Auchan<-fromJSON(judete_Auchan)
###################################################################################
#Auchan actual hypermarkets
###################################################################################
magazine_Auchan<-regmatches(html_text(magazine_Auchan),
                          gregexpr("(?<=var jsData = ).*?(?=];)", html_text(magazine_Auchan), perl = TRUE)
)
magazine_Auchan<-as.character(magazine_Auchan)
magazine_Auchan<-paste(magazine_Auchan,"]")
magazine_Auchan<-fromJSON(magazine_Auchan)
###################################################################################
                                    #Carrefour
###################################################################################
magazine_Carrefour<-read_html("data_html/carrefour.html") #Carrefour HTML file contents
###################################################################################
magazine_Carrefour<-regmatches(html_text(magazine_Carrefour),
                            gregexpr("(?<=var coordonate_magazine = ).*?(?=];)", html_text(magazine_Carrefour), perl = TRUE)
                              )
magazine_Carrefour<-as.character(magazine_Carrefour)
magazine_Carrefour<-paste(magazine_Carrefour,"]")
magazine_Carrefour<-fromJSON(magazine_Carrefour)

# ia din prima coloana numele localitatii si il adauga in coloana a 5-a
for(i in 1:nrow(magazine_Carrefour)){ 
   for(j in magazine_Carrefour[[i]]){
      magazine_Carrefour[i,5]<-substr(magazine_Carrefour[[i]], 
             which(strsplit(magazine_Carrefour[[i]],"")[[1]]=="/")[1]+1,
             which(strsplit(magazine_Carrefour[[i]],"")[[1]]=="/")[2]-1
      )
   }
   rm(i,j)
}
magazine_Carrefour<-as.data.frame(magazine_Carrefour)
###################################################################################
                                     #Cora
###################################################################################
magazine_Cora<-read_html("data_html/cora.html") #Cora HTML file contents
###################################################################################
#in progress, tomorrow is another day ^^
