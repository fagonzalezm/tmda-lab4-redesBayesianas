require(bnlearn)
require(bnviewer)

bn_df <- data.frame(insurance)
summary(bn_df)
# 20000 observaciones
# 27 variables
# Datos categóricos
# Los datos son factores de hasta 5 niveles

res <- hc(bn_df) #Algoritmo Hill-Climbing

viewer(res,
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

bl <- matrix(c("ThisCarCost","ThisCarDam","ThisCarDam","ThisCarCost","MedCost","ThisCarDam","MakeModel","VehicleYear","VehicleYear","MakeModel","VehicleYear","RiskAversion","RiskAversion","VehicleYear","SocioEcon","OtherCar","OtherCar","SocioEcon","RuggedAuto","MakeModel","MakeModel","OtherCar","OtherCar","MakeModel","Mileage","ThisCarDam","ThisCarDam","Mileage","RiskAversion","HomeBase","Airbag","Cushioning","Cushioning","Airbag"),ncol=2,byrow=TRUE)
res <- hc(bn_df,blacklist = bl)
viewer(res,
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

print(res)

sc<-score(res,bn_df) # BIC por default
print(sc)


fittedbn <- bn.fit(res, data = bn_df) # Se obtiene la tabla de probabilidades condicionales mediante EM. (Máxima Expectación, propagación de la evidencia)
print(fittedbn$Age) #se obtiene la información respecto del nodo Proteins


cpquery(fittedbn, event = (Proteins=="<3"), evidence = ( Smoking=="no") ) 

cpquery(fittedbn, event = (Pressure==">140"), evidence = ( Proteins=="<3" ) )

require(bnlearn)
bn_df <- data.frame(asia)


res <- hc(bn_df)
plot(res)
sc<-score(res,bn_df)
print(sc)


fittedbn <- bn.fit(res, data = bn_df)
print(fittedbn$E)


cpquery(fittedbn, event = (B=="yes"), evidence = ( S=="no") ) 

cpquery(fittedbn, event = (B=="yes"), evidence = ( S=="yes") )

bn_df <- data.frame(asia)
res <- mmhc(bn_df)
plot(res)
sc<-score(res,bn_df)
print(sc)


fittedbn <- bn.fit(res, data = bn_df) #idéntico al anterior
print(fittedbn$E)

cpquery(fittedbn, event = (B=="yes"), evidence = ( S=="no") )

cpquery(fittedbn, event = (B=="yes"), evidence = ( S=="yes") )

bn_df <- data.frame(asia)
res <- mmpc(bn_df)
plot(res)

fittedbn <- bn.fit(res, data = bn_df) #no hay direccionalidad en los arcos