
rm(list=ls())


rdata = read.csv("AidDataCoreThin_ResearchRelease_Level1_v3.0.csv")
GDP = read.csv("API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv")


AidPanel <- NULL

countries <- list("Ethiopia","Afghanistan","Iraq","Bangladesh","Paraguay","Brazil","South Africa","Indonesia","India","Sudan","Cuba","Malawi",
                  "Nigeria","Algeria","Swaziland","Botswana","Uganda","Morocco","Kenya","Zimbabwe","Madagascar","Bulgaria","Guinea","Liberia")
reg <- function(b) {
  for (i in 1:24) {
    country = toString(countries[i])
    for (j in 2002:2013) {
      year = j
      data2 = data[data$year==year & data$recipient == country & data$coalesced_purpose_name != "Sectors not specified",]
      
      data3 = data2[,c(-1,-2,-4,-7)]
      
      data3$DummyFood = 1*(data3$coalesced_purpose_name == "Emergency food aid" | data3$coalesced_purpose_name == "Food aid/Food security programmes" 
                           | data3$coalesced_purpose_name == "Basic nutrition")
      data3$DummySupply = 1*(data3$coalesced_purpose_name == "Material relief assistance and services" | data3$coalesced_purpose_name == "Agricultural inputs"
                             | data3$coalesced_purpose_name == "Agricultural land resources")
      data3$DummyHealth = 1*(data3$coalesced_purpose_name == "Basic health care" | data3$coalesced_purpose_name == "Basic health infrastructure" 
                             | data3$coalesced_purpose_name == "Emergency health services/support" | data3$coalesced_purpose_name == "Family planning"
                             | data3$coalesced_purpose_name == "Health personnel development" | data3$coalesced_purpose_name == "Health policy and administrative management"
                             | data3$coalesced_purpose_name == "Health, combination of general, basic, and population policy/reproductive health purposes"
                             | data3$coalesced_purpose_name == "Health, general, combinations of activities" | data3$coalesced_purpose_name == "Health, purpose unspecified or does not fit under any other applicable codes" 
                               | data3$coalesced_purpose_name == "Infectious & Parasitic disease control" | data3$coalesced_purpose_name == "Medical services"
                             | data3$coalesced_purpose_name == "Population policies/ programmes and reproductive health, combinations of activities" 
                             | data3$coalesced_purpose_name == "Personnel development for population and reproductive health"
                             | data3$coalesced_purpose_name == "Reproductive health care" | data3$coalesced_purpose_name == "STD control including HIV/AIDS")
      data3$DummyAdmin = 1*(data3$coalesced_purpose_name == "Administrative costs" | data3$coalesced_purpose_name == "Agricultural policy and administrative management"
                            | data3$coalesced_purpose_name == "Employment policy and administrative management" | data3$coalesced_purpose_name == "Environmental policy and administrative management"
                            | data3$coalesced_purpose_name == "Energy policy and administrative management" | data3$coalesced_purpose_name == "Financial policy and administrative management"
                            | data3$coalesced_purpose_name == "Government administration" | data3$coalesced_purpose_name == "Industrial policy and administrative management"
                            | data3$coalesced_purpose_name == "Population policy and administrative management")
      data3$DummyEduc = 1*(data3$coalesced_purpose_name == "Basic life skills for youth and adults" | data3$coalesced_purpose_name == "Early childhood education"
                           | data3$coalesced_purpose_name == "Education facilities and training" | data3$coalesced_purpose_name == "Education policy and administrative management"
                           | data3$coalesced_purpose_name == "Education, level unspecified, purpose unspecified or does not fit under any other applicable codes" | data3$coalesced_purpose_name == "Health education"
                           | data3$coalesced_purpose_name == "Higher education" | data3$coalesced_purpose_name == "Mining Education / Training" 
                           | data3$coalesced_purpose_name == "Multisector education/training" | data3$coalesced_purpose_name == "Secondary education"
                           | data3$coalesced_purpose_name == "Primary education" | data3$coalesced_purpose_name == "Teacher training"
                           | data3$coalesced_purpose_name == "Vocational training" | data3$coalesced_purpose_name ==  "Agricultural education/training"
                           | data3$coalesced_purpose_name == "Medical education/training")
      data3$DummyInfra = 1*(data3$coalesced_purpose_name == "Agricultural development" | data3$coalesced_purpose_name == "Agricultural services, purpose"
                            | data3$coalesced_purpose_name == "Agricultural water resources" | data3$coalesced_purpose_name == "Basic drinking water supply and basic sanitation"
                            | data3$coalesced_purpose_name == "Electrical transmission/ distribution"
                            | data3$coalesced_purpose_name == "Energy generation and supply, combinations of activities" | data3$coalesced_purpose_name == "Forestry development"
                            | data3$coalesced_purpose_name == "Information and communication technology (ICT)" | data3$coalesced_purpose_name == "Power generation/renewable sources"
                            | data3$coalesced_purpose_name == "Air transport"| data3$coalesced_purpose_name == "Rail transport"
                            | data3$coalesced_purpose_name == "River development" | data3$coalesced_purpose_name == "Road transport"
                            | data3$coalesced_purpose_name == "Rural development")
      data3$DummyEcon = 1*(data3$coalesced_purpose_name == "Debt forgiveness" | data3$coalesced_purpose_name == "Economic and development policy/planning"
                           | data3$coalesced_purpose_name == "Formal sector financial intermediaries" | data3$coalesced_purpose_name == "General budget support"
                           | data3$coalesced_purpose_name == "Informal/semi-formal financial intermediaries" | data3$coalesced_purpose_name == "Public sector financial management"
                           | data3$coalesced_purpose_name == "Small and medium-sized enterprises (SME) development"
                           | data3$coalesced_purpose_name == "Business support services and institutions")
      data3$DummyWelfare = 1*(data3$coalesced_purpose_name == "Social/ welfare services" | data3$coalesced_purpose_name == "Strengthening civil society"
                              |  data3$coalesced_purpose_name == "Women in development" | data3$coalesced_purpose_name == "Legal and judicial development")
      data3$DummyWar = 1*(data3$coalesced_purpose_name == "Child soldiers (Prevention and demobilisation)" | data3$coalesced_purpose_name == "Civilian peace-building, conflict prevention and resolution"
                          | data3$coalesced_purpose_name == "Land mine clearance" | data3$coalesced_purpose_name == "Post-conflict peace-building (UN)")
      
      FoodAid = sum(data3$commitment_amount_usd_constant[data3$DummyFood == 1])
      SupplyAid = sum(data3$commitment_amount_usd_constant[data3$DummySupply == 1])
      HealthAid = sum(data3$commitment_amount_usd_constant[data3$DummyHealth == 1])
      AdminAid = sum(data3$commitment_amount_usd_constant[data3$DummyAdmin== 1])
      EducAid = sum(data3$commitment_amount_usd_constant[data3$DummyEduc == 1])
      InfraAid = sum(data3$commitment_amount_usd_constant[data3$DummyInfra == 1])
      EconAid = sum(data3$commitment_amount_usd_constant[data3$DummyEcon == 1])
      WelfareAid = sum(data3$commitment_amount_usd_constant[data3$DummyWelfare == 1])
      WarAid = sum(data3$commitment_amount_usd_constant[data3$DummyWar == 1])
      
      Aid = data.frame(year,country,FoodAid,SupplyAid,HealthAid,AdminAid,EducAid,InfraAid,EconAid,WelfareAid,WarAid)
      
      AidPanel = rbind(AidPanel,Aid)
      
    }
  }
  return(AidPanel)
}

x = reg(1)

GDP1 = NULL
for (i in 1:24) {
  country = toString(countries[i])
  data4 = GDP[GDP$Country.Name==country,]
  for (j in 48:59) {
    GDP1 = rbind(GDP1,(data4[1,j]-data4[1,j-1])/data4[1,j-1])
  }
}


Reg <- cbind(GDP1,x)
pReg<-pdata.frame(Reg,index=c("country","year"))

ppool <- plm(GDP1~FoodAid+SupplyAid+HealthAid+AdminAid+EducAid+InfraAid+EconAid+WelfareAid+WarAid+1, data = pReg, model = "pooling")
pfe <- plm(GDP1~FoodAid+SupplyAid+HealthAid+AdminAid+EducAid+InfraAid+EconAid+WelfareAid+WarAid+1, data = pReg, model = "within",effect="time")
pre <- plm(GDP1~FoodAid+SupplyAid+HealthAid+AdminAid+EducAid+InfraAid+EconAid+WelfareAid+WarAid+1, data = pReg, model = "random")

summary(ppool)
summary(pfe)
summary(pre)

phtest(pfe,pre)

#Fails to Rejects null, Supports both FE and RE Model, FE is more efficient as its standard error seems to be marginally smaller, thus we will be using this model.
#Almost every Varibale is statisticalyl insignificant. Supports the results of the paper. Foreign aid is just as ineffective at increasing GDP growth as it was 
#20 years ago




