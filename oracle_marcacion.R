# Oracle connection - Marcacion
#==================================
library(stringr)
library(RJDBC)
library(dplyr)

# Oracle connection
jdbdriver<-JDBC("oracle.jdbc.OracleDriver",classPath="C:/Users/dlazo/Downloads/ojdbc11.jar")
jdbcConnection =dbConnect(jdbdriver, 
                          "jdbc:oracle:thin:@//aasal2.aasa.com.pe:1521/aasap",
                          "Consulta", "AC2802")
query1<-dbGetQuery(jdbcConnection,
                   "SELECT ID_MARCACION_TER 
                    FROM PORTALTER.PTERCER_MARCACION_TERCERIA ROWNUM<6;") 
query1
