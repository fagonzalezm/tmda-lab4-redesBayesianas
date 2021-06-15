require(bnlearn)
require(bnviewer)

bn_df <- data.frame(insurance)
summary(bn_df)
# 20000 observaciones
# 27 variables
# Datos categóricos
# Los datos son factores de hasta 5 niveles

#
bn_df <- bn_df[,-13] # Se elimina DrivSkill porque es muy parecido a drivQuality, es redundante
bn_df <- bn_df[,-15] # Se elimina Theft porque son casi puros false

hc <- hc(bn_df) #Algoritmo Hill-Climbing

viewer(hc,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_on_grid",
       bayesianNetwork.title="Discrete Bayesian Network - Insurance",
       bayesianNetwork.subtitle = "Car insurance",
       bayesianNetwork.footer = "Fig. 2 - Layout on grid",
       
       node.colors = list(background = "#00722e",
                          border = "#484848",
                          highlight = list(background = "#00b347",
                                           border = "#008f39"))
)

hc.sc<-score(hc,bn_df) # BIC por default
print(hc.sc)

# mmhc
mmhc<-mmhc(bn_df)
mmhc.sc <- score(mmhc,bn_df)
print(mmhc.sc)
viewer(mmhc,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_on_grid",
       bayesianNetwork.title="Discrete Bayesian Network - Insurance MMHC",
       bayesianNetwork.subtitle = "Car insurance",
       bayesianNetwork.footer = "Fig. 2 - Layout on grid",
       
       node.colors = list(background = "#00722e",
                          border = "#484848",
                          highlight = list(background = "#00b347",
                                           border = "#008f39"))
)

# mmpc
mmpc<-mmpc(bn_df)
#mmpc.sc <- score(mmpc,bn_df)
viewer(mmpc,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_on_grid",
       bayesianNetwork.title="Discrete Bayesian Network - Insurance MMPC",
       bayesianNetwork.subtitle = "Car insurance",
       bayesianNetwork.footer = "Fig. 2 - Layout on grid",
       
       node.colors = list(background = "#00722e",
                          border = "#484848",
                          highlight = list(background = "#00b347",
                                           border = "#008f39"))
)

# hc tiene mator BIC, se elige ese método
# Se creó una blacklist a partir del modelo hc y conocimiento sobre el problema.
bl <- matrix(c("ThisCarCost","ThisCarDam","ThisCarDam","ThisCarCost","MedCost","ThisCarDam","MakeModel","VehicleYear","VehicleYear","MakeModel","VehicleYear","RiskAversion","RiskAversion","VehicleYear","SocioEcon","OtherCar","OtherCar","SocioEcon","RuggedAuto","MakeModel","MakeModel","OtherCar","OtherCar","MakeModel","Mileage","ThisCarDam","ThisCarDam","Mileage","RiskAversion","HomeBase","Airbag","Cushioning","Cushioning","Airbag"),ncol=2,byrow=TRUE)
print(bl)
res <- hc(bn_df,blacklist = bl)
viewer(res,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_on_grid",
       bayesianNetwork.title="Discrete Bayesian Network - Insurance HC",
       bayesianNetwork.subtitle = "Car insurance",
       bayesianNetwork.footer = "Fig. 2 - Layout on grid",
       
       node.colors = list(background = "#00722e",
                          border = "#484848",
                          highlight = list(background = "#00b347",
                                           border = "#008f39"))
)
print(res)


###### CONSULTAS ########

# Problema: ¿Contar con medidas de seguridad en los autos aumenta o disminuye los accidentes automovilístiocos?¿Influye en la gravedad de estos?
