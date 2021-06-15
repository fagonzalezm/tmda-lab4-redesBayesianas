require(bnlearn)
require(bnviewer)

bn_df <- data.frame(insurance)
summary(bn_df)
# 20000 observaciones
# 27 variables
# Datos categóricos
# Los datos son factores de hasta 5 niveles

#
DrivingSkill <- bn_df$DrivSkill
drivQuality <- bn_df$DrivQuelity

DrivingSkill <- factor(bn_df$DrivingSkill, levels=c("SubStandard","Normal","Expert"), labels=c(1,2,3))
DrivQuality <- factor(bn_df$DrivQuality, levels=c("Poor","Normal","Excellent"), labels=c(1,2,3))
data.cor <- chisq.test(DrivingSkill, DrivQuality)
print(data.cor)

bn_df <- bn_df[,-13] # Se elimina drivSkill porque es muy parecido a drivQuality, es redundante
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

#Se entrena el método
set.seed(20)
res.fitted <- bn.fit(res,data = bn_df) # Se obtiene la tabla de probabilidades condicionales mediante EM. (Máxima Expectación, propagación de la evidencia)
res.fitted

###### CONSULTAS ########
# Problema: ¿Contar con medidas de seguridad en los autos aumenta o disminuye la severidad de los accidentes automovilístiocos?¿Influyen en la conducta de los conductores?
# Hipótesis de la investigación: Contar con sistemas de seguridad disminuye los accidentes automovilísticos y su gravedad.

#
# a) ¿Ruggeduto, Antilock, Cushioning y Airbag influyen en severidad de accidentes?
#
#Funciona al revés de lo esperado
cpquery(res.fitted,evidence = (RuggedAuto == "Tank"),event = (Accident == "None"))      # 0.662248
cpquery(res.fitted,evidence = (RuggedAuto == "Football"),event = (Accident == "None"))  # 0.7229405
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell"),event = (Accident == "None"))  # 0.7281314

cpquery(res.fitted,evidence = (Antilock == "True"),event = (Accident == "None"))        # 0.7747475
cpquery(res.fitted,evidence = (Antilock == "False"),event = (Accident == "None"))       # 0.7051005

# Funciona al revés de lo esperado
cpquery(res.fitted,evidence = (Cushioning == "Excellent"),event = (Accident == "None")) # 0.6929825
cpquery(res.fitted,evidence = (Cushioning == "Good"),event = (Accident == "None"))      # 0.7125066
cpquery(res.fitted,evidence = (Cushioning == "Fair"),event = (Accident == "None"))      # 0.7307078
cpquery(res.fitted,evidence = (Cushioning == "Poor"),event = (Accident == "None"))      # 0.7315341

cpquery(res.fitted,evidence = (Airbag == "True"),event = (Accident == "None"))          # 0.7313573
cpquery(res.fitted,evidence = (Airbag == "False"),event = (Accident == "None"))         # 0.7107742

# Se estudia RuggedAuto
cpquery(res.fitted,evidence = (RuggedAuto == "Tank"),event = (Accident == "None"))      # 0.6523711
cpquery(res.fitted,evidence = (RuggedAuto == "Tank"),event = (Accident == "Mild"))      # 0.08074534
cpquery(res.fitted,evidence = (RuggedAuto == "Tank"),event = (Accident == "Moderate"))  # 0.0941704
cpquery(res.fitted,evidence = (RuggedAuto == "Tank"),event = (Accident == "Severe"))    # 0.1767347

cpquery(res.fitted,evidence = (RuggedAuto == "Football"),event = (Accident == "None"))          # 0.7163692
cpquery(res.fitted,evidence = (RuggedAuto == "Football"),event = (Accident == "Mild"))          # 0.09117027
cpquery(res.fitted,evidence = (RuggedAuto == "Football"),event = (Accident == "Moderate"))      # 0.07062844
cpquery(res.fitted,evidence = (RuggedAuto == "Football"),event = (Accident == "Severe"))        # 0.1141561

cpquery(res.fitted,evidence = (RuggedAuto == "EggShell"),event = (Accident == "None"))          # 0.7293622
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell"),event = (Accident == "Mild"))          # 0.09044552
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell"),event = (Accident == "Moderate"))      # 0.0772123
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell"),event = (Accident == "Severe"))        # 0.1153485

# Poniendo los factores que tienen resultados distintos a los esperados baja aún mas la probabilidad que muestra el modelo para que el accidente tenga menos gravedad
cpquery(res.fitted,evidence = (RuggedAuto == "Tank" & Cushioning == "Excellent"),event = (Accident == "None"))          # 0.6927224
cpquery(res.fitted,evidence = (RuggedAuto == "Tank" & Cushioning == "Excellent"),event = (Accident == "Mild"))          # 0.07061959
cpquery(res.fitted,evidence = (RuggedAuto == "Tank" & Cushioning == "Excellent"),event = (Accident == "Moderate"))      # 0.07762557
cpquery(res.fitted,evidence = (RuggedAuto == "Tank" & Cushioning == "Excellent"),event = (Accident == "Severe"))        # 0.1558785

cpquery(res.fitted,evidence = (RuggedAuto == "EggShell" & Cushioning == "Poor"),event = (Accident == "None"))           # 0.7322335
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell" & Cushioning == "Poor"),event = (Accident == "Mild"))           # 0.09442891
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell" & Cushioning == "Poor"),event = (Accident == "Moderate"))       # 0.07302659
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell" & Cushioning == "Poor"),event = (Accident == "Severe"))         # 0.1083924

#
# b) ¿Ruggeduto, Antilock, Cushioning y Airbag influyen en RiskAversion?
#

cpquery(res.fitted,evidence = (RuggedAuto == "Tank"),event = (RiskAversion == "Adventurous"))
cpquery(res.fitted,evidence = (RuggedAuto == "Football"),event = (RiskAversion == "Adventurous"))
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell"),event = (RiskAversion == "Adventurous"))

cpquery(res.fitted,evidence = (Antilock == "True"),event = (RiskAversion == "Adventurous"))
cpquery(res.fitted,evidence = (Antilock == "False"),event = (RiskAversion == "Adventurous"))

# Funciona al revés de lo esperado
cpquery(res.fitted,evidence = (Cushioning == "Excellent"),event = (RiskAversion == "Adventurous"))
cpquery(res.fitted,evidence = (Cushioning == "Good"),event = (RiskAversion == "Adventurous"))
cpquery(res.fitted,evidence = (Cushioning == "Fair"),event = (RiskAversion == "Adventurous"))
cpquery(res.fitted,evidence = (Cushioning == "Poor"),event = (RiskAversion == "Adventurous"))

cpquery(res.fitted,evidence = (Airbag == "True"),event = (RiskAversion == "Adventurous"))
cpquery(res.fitted,evidence = (Airbag == "False"),event = (RiskAversion == "Adventurous"))

# Poniendo los factores que tienen resultados distintos a los esperados baja aún mas la probabilidad que muestra el modelo para que el accidente tenga menos gravedad
cpquery(res.fitted,evidence = (RuggedAuto == "Tank" & Cushioning == "Excellent"),event = (RiskAversion == "Cautious"))
cpquery(res.fitted,evidence = (RuggedAuto == "Tank" & Cushioning == "Excellent"),event = (RiskAversion == "Normal"))
cpquery(res.fitted,evidence = (RuggedAuto == "Tank" & Cushioning == "Excellent"),event = (RiskAversion == "Adventurous"))
cpquery(res.fitted,evidence = (RuggedAuto == "Tank" & Cushioning == "Excellent"),event = (RiskAversion == "Psychopath"))

cpquery(res.fitted,evidence = (RuggedAuto == "EggShell" & Cushioning == "Poor"),event = (RiskAversion == "Cautious"))
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell" & Cushioning == "Poor"),event = (RiskAversion == "Normal"))
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell" & Cushioning == "Poor"),event = (RiskAversion == "Adventurous"))
cpquery(res.fitted,evidence = (RuggedAuto == "EggShell" & Cushioning == "Poor"),event = (RiskAversion == "Psychopath"))


