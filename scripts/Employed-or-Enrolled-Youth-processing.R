library(dplyr)
library(datapkg)
library(acs)
library(data.table)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Employed or Enrolled Youth
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

# ACS B14005
# Get geography object for CT and subcounty divisions
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2017,
    table = "B14005"
)

options(scipen=9999)
dataset <- data.table()
for (data in acsdata) {
    year <- data@endyear
    year <- paste(as.numeric(year)-4, as.numeric(year), sep="-")
 # Totals for denominator    
    pop.total <- acsSum(data, 1, "Total")
    pop.total.m <- acsSum(data, 2, "Total Male")
    pop.total.f <- acsSum(data, 16, "Total Female")
 # aggregate values
    #first sum appropriately
    pop.youth.male <- acsSum(data, c(3, 9, 13), "Employed or Enrolled Male")
    pop.youth.female <- acsSum(data, c(17, 23, 27), "Employed or Enrolled Female")
    pop.youth.total <- acsSum(data, c(3, 9, 13, 17, 23, 27), "Employed or Enrolled Total")
    #then divide appropriately 
    percent.male <- divide.acs(pop.youth.male, pop.total.m, method = "proportion")
    acs.colnames(percent.male) <- "Percent Employed or Enrolled Male"
    percent.female <- divide.acs(pop.youth.female, pop.total.f, method = "proportion")
    acs.colnames(percent.female) <- "Percent Employed or Enrolled Female"
    percent.total <- divide.acs(pop.youth.total, pop.total, method = "proportion")
    acs.colnames(percent.total) <- "Percent Employed or Enrolled Total" 
    
 # merge in fips
    datafips <- data.table(geography(data)[2])
    
 # Cast to separate data frames
    numberEstimates <- data.table(
        datafips$Id2,
        estimate(pop.total),
        estimate(pop.total.m),
        estimate(pop.total.f),
        estimate(pop.youth.male),
        estimate(pop.youth.female),
        estimate(pop.youth.total),
        year,
        "Number",
        "Employed or Enrolled Youth"
    )
    numberMOES <- data.table(
        datafips$Id2,
        standard.error(pop.total) * 1.645,
        standard.error(pop.total.m) * 1.645,
        standard.error(pop.total.f) * 1.645,
        standard.error(pop.youth.male) * 1.645,
        standard.error(pop.youth.female) * 1.645,
        standard.error(pop.youth.total) * 1.645,        
        year,
        "Number",
        "Margins of Error"
    )
    numberNames <- c(
            "FIPS",
            "Number:Total:Total",
            "Number:Total:Male",
            "Number:Total:Female",
            "Number:Youth:Male",
            "Number:Youth:Female",
            "Number:Youth:Total",            
            "Year",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
        rbind(numberEstimates, numberMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Employed or Enrolled Youth",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    percentEstimates <- data.table(
        datafips$Id2,
        estimate(percent.male),
        estimate(percent.female),
        estimate(percent.total),
        year,
        "percent",
        "Employed or Enrolled Youth"
    )
    percentMOES <- data.table(
        datafips$Id2,
        standard.error(percent.male) * 1.645,
        standard.error(percent.female) * 1.645,
        standard.error(percent.total) * 1.645,        
        year,
        "percent",
        "Margins of Error"
    )
    percentNames <- c(
        "FIPS",
        "Percent:Youth:Male",
        "Percent:Youth:Female",
        "Percent:Youth:Total",
        "Year",
        "Measure Type",
        "Variable"
     )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)

    percentsData.melt <- melt(
        rbind(percentEstimates, percentMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Employed or Enrolled Youth",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    dataset <- rbind(dataset, numbersData.melt, percentsData.melt)
}

# Parse gender, variable out of "var" column
dataset[,c("Measure Type", "Type", "Gender"):=do.call(Map, c(f = c, strsplit(`Employed or Enrolled Youth`, ":", fixed = T)))]

dataset <- dataset[dataset$Type == "Youth",]

dataset$Type <- NULL
dataset$`Employed or Enrolled Youth` <- NULL

dataset$FIPS <- gsub("^", "0", dataset$FIPS)

#Merge in towns by FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

dataset <- merge(towns, dataset, by = "FIPS", all.x=T)

# Round Values according to type/variable
dataset <- as.data.table(dataset)
dataset[`Measure Type` == "Number", Value := round(Value, 0)]
dataset[`Measure Type` != "Number", Value := round(Value*100, 2)]

dataset$Value[dataset$Value == "NaN"] <- NA

dataset$Gender <- factor(dataset$Gender, levels = c("Total", "Female", "Male"))

# Select and sort columns
dataset <- dataset %>% 
  select(Town, FIPS, Year, Gender, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, Gender, `Measure Type`, Variable)

# write to table
write.table(
    dataset,
    file.path(getwd(), "data", "employed-or-enrolled-youth-2017.csv"),
    sep = ",",
    row.names = F, 
    na="-9999"
)
