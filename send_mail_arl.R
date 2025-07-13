# Envio de correo automatico ARL
#--------------------------------
library(dplyr)
library(stringr)
library(lubridate)
library(xtable)
library(RDCOMClient)

path<-"Z:/" #M:/ local Z:/ server
setwd(path)
df_resmail<-data.frame(Fecha=c(),Hora=c(),Colada=c(),Tipo=c(),ARL=c(),Cr=c(),Ni=c(),Cu=c(),Sn=c(),S=c(),Pb=c())
indc<-0

for(i in 1:2628000){
  
  possibleError<-tryCatch({
    # Indicador de reinicio por fecha
    dte_h<-weekdays(Sys.Date())
    fech_hr1<-ifelse(dte_h=="sábado", "20:30:00","20:45:00")
    
    # Carga de Archivos
    data1<-read.csv("RESULTAD.csv") #RESULTAD - copia
    dt1_tp<-names(data1[9]) 
    lup1<-file.info(paste0(path,"/RESULTAD.csv"))$mtime
    
    data2<-read.csv("RESULTA2.csv") #RESULTAD - copia
    dt2_tp<-names(data2[9])
    lup2<-file.info(paste0(path,"/RESULTA2.csv"))$mtime
    
    c<-Sys.time() %>% as.character()
    c1<-gsub("-","",c)
    c2<-gsub(" ","_",c1)
    c3<-gsub(":","_",c2)
    
    ## Fechas de corte
    if(format(Sys.time(),"%H:%M")==substr(fech_hr1,1,5)){
      indc=0
    }
    difftime(Sys.time(), lup2 ,units="hours")
    
    if(difftime(Sys.time(), lup1 ,units="hours")<4 &day(lup1)==day(Sys.time())){
      data_s<-data1
      lups<-lup1
    }else if(difftime(Sys.time(), lup2 ,units="hours")<4 &day(lup2)==day(Sys.time())){
      data_s<-data2
      lups<-lup2
    }
    
    #dim(data1)[2]>50
    if(difftime(Sys.time(), lups ,units="secs")<60 & dt1_tp=="FC1"){ #lup1
      
      # Data 1
      df_b1<-data.frame()
      data_df1<-rbind(df_b1,colnames(data_s))  #data1
      data11<-data_df1[,c(6:8,9,10,17,19:32,40)] #17 num arl
      names(data11)<-c("Fecha","Hora","Colada","Tipo","num_orden","ARL",
                       "C","Mn","Si","P", "S", "Cr","Ni","Mo","Cu",
                       "Sn","V","Al","Nb","Ti","Pb")
      data12<-data11[,c(1:4,6,12,13,15,16,11,21,14)]
      data12[,-c(3,4,5)]<-sapply(data12[,-c(3,4,5)],str_sub,2,-1)
      data12[,c(6:11)]<-sapply(data12[,c(6:11)],as.numeric)
      data12$Hora<-gsub("\\.",":",data12$Hora)
      
      #########################
      if(data12$Hora>= fech_hr1 &  indc==0 ){
        df_resmail<-data.frame(Fecha=c(),Hora=c(),Colada=c(),Tipo=c(),ARL=c(),Cr=c(),Ni=c(),Cu=c(),Sn=c(),S=c(),Pb=c())
      } else {
        df_resmail<-df_resmail
      }
      ########################
      
      df_resmail<-rbind(df_resmail,data12)
      Outlook <- COMCreate("Outlook.Application")
      Email = Outlook$CreateItem(0)
      df_resmail<-df_resmail[!duplicated(df_resmail$Hora),]
      df_resmail<-df_resmail[df_resmail$Tipo=="FC1",]
      
      df_resmail[,c(6:11)]<-sapply(df_resmail[,c(6:11)],round,4)
      dfaux<-xtable(df_resmail)
      digits(dfaux) <- c(0,0,0,0,0,0,2,2,2,3,rep(3,3))
      y<-print(dfaux, type="html",include.rownames=FALSE,print.results=FALSE)
      
      #y <- print(xtable(df_resmail), type="html", print.results=FALSE,include.rownames=FALSE)
      # Email[["to"]] = "dlazo@acerosarequipa.com;croldan@acerosarequipa.com;vperezp@acerosarequipa.com;
      # aquispe@acerosarequipa.com;ksierraal@acerosarequipa.com;lmiranda@acerosarequipa.com;
      #     pmuniz@acerosarequipa.com;dguzman@acerosarequipa.com;bgarcia@acerosarequipa.com;
      #     rdirecta@acerosarequipa.com;mberrio@acerosarequipa.com;
      #     lmiranda@acerosarequipa.com;mabravo@acerosarequipa.com;rflores@acerosarequipa.com;
      #     vormeno@acerosarequipa.com;hcontrer@acerosarequipa.com;balces@acerosarequipa.com"  #croldan@aasa.com.pe;ksierraal@aasa.com.pe;ldiaz@aasa.com.pe ;solaya@aasa.com.pe; 
      Email[["to"]] ="dlazo@acerosarequipa.com"
      Email[["cc"]] = ""
      Email[["bcc"]] = ""
      Email[["subject"]] = paste0("SecuenciaResiduales_",c3)
      Email[["htmlbody"]] =paste0(
        "<h3>Resultados Quimicos ARL (arl1)</h3>",
        "<p>  
               </p>",
        "<html>", y, "</html>")
      
      if(difftime(Sys.time(), lup1 ,units="secs")<50){
        Email$Send()
        indc<-indc+1
      }
      
      rm(Outlook, Email)
      print("se ejecuto programa ARL1")
      
    }else {
      
      print(paste0("tipo de resultado ",dt1_tp))
    }
    
  },error = function(e) {e 
    print(paste0("ERROR :"," ",Sys.time()))})
  if(inherits(possibleError, "error")) next 
  Sys.sleep(50)
}






