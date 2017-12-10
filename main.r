###################################################################################
#Statistici referitoare la hipermarket-urile din Romania
###################################################################################
project_path<-getwd() #locatia proiectului
source(paste(project_path,"Data preparation scripts","prerequisites.r",sep="\\")) #include fisierul de pre-requisites
source(paste(project_path,"Data preparation scripts","populatie.r",sep="\\")) #include fisierul ce extrage si manipuleaza datele despre populatia oraselor din Romania
source(paste(project_path,"Data preparation scripts","hipermarketuri.r",sep="\\")) #include fisierul ce extrage si manipuleaza datele despre hipermarketurile din Romania
###################################################################################
                                 #Graphs creation
###################################################################################
                     #Treemap creation - Hypermarkets per capita
###################################################################################
treemap_hypermarket_per_capita<-ggplot(
   count(hypermarkets, "Location"=hypermarkets$Localitati, "Population"=hypermarkets$Total), 
   aes(
      label = Location,
      area = (n/Population), 
      fill = (Population/n)#,
   )) +
   geom_treemap()+
   geom_treemap_text(fontface = "italic", colour = "#ECF3F7", place = "center",
                     grow = F, reflow = T)+
   theme(legend.position = "right")+
   scale_fill_gradient(low="#941714",high="#1D9414")+
   labs(
      title = "Hypermarkets per capita in Romania's main cities",
      caption = "The smaller the square it is, the less hypermarkets/capita exists.",
      fill = "Hypermarket per population"
   )
plot(treemap_hypermarket_per_capita)
###################################################################################
