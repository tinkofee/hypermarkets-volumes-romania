###################################################################################
#Statistici referitoare la hipermarket-urile din Romania
###################################################################################
project_path<-getwd() #locatia proiectului
source(paste(project_path,"Data preparation scripts","prerequisites.r",sep="\\")) #include fisierul de pre-requisites
source(paste(project_path,"Data preparation scripts","populatie.r",sep="\\")) #include fisierul ce extrage si manipuleaza datele despre populatia oraselor din Romania
source(paste(project_path,"Data preparation scripts","hipermarketuri.r",sep="\\")) #include fisierul ce extrage si manipuleaza datele despre hipermarketurile din Romania
###################################################################################