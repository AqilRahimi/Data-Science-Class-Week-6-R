#1. Create Dataframe
names =c("Carrol","Mike","John")
gender = c("Female", "Male", "Male")
height = c(160, 175, 173)
weight = c(49, 89, 77)
age = c(35, 36, 41)
df = data.frame(names,gender,height,weight,age)
View(df)

df[1,3] #row 1, column 3 
df[1:2, 1:3] #row 1 until 2, column 1 until 3
df["height"]
df["gender"] #appear gender by column
df$gender #appear gender in one row
df[1:2]
df[c(1,2)]
df[df$names=="Mike",]
df[df$names=="Fufu",]

#rbind(combining row), cbind(combining column)

#2.add rows
newdf= rbind(df,data.frame(names = "Suuria",                            gender = "Female",
                           height = 156,
                           weight = 56,
                           age = 23 ))
#print("After Added rows:\n")
print(newdf)
print("Before Adding:\n")
print(df)
newDf = rbind(df, data.frame(names = c("Suuria","Malik"),
                             gender = c("Female","Male"),
                             height = c(156,169),
                             weight = c(56,81),
                             age = c(23, 28)))
print("After Added rows:\n")
print(newDf)

#3.merging
gender = c("Female", "Male", "Male","Female","Male")
height = c(160, 175, 173,156,169)
weight = c(49, 89, 77,56,81)
age = c(35, 36, 41,23, 28)
names =c("Carrol","Mike","John","Suuria","Malik")
df1 = data.frame(names,gender,height,weight,age)

states = c("Selangor","Perak","Melaka")
names = c("Carrol","John","Malik")
df2 = data.frame(names, states)

dfMerge= merge(df1, df2, by = "names", all= TRUE)
View(dfMerge)

#4.removing rows
newDf2 = newDf[-2,]
newDf3 = newDf
newDf3[1,"age"] <- 30
newDf3[4, "height"] <- 152

#5.remove column 4
newDf4 = newDf
newDf4[,-4]

#6.import dataframe
data_cv <- read.csv("C:/Users/HP/Desktop/algorithm-data-structure/data_science/DATASCIENCECLASS/my_covid.csv")
summary(data_cv)

#-seq is not including row number
newdf<-data_cv[-seq(10,13), ] 
View(newdf)
write.csv(newdf, "C:/Users/HP/Desktop/algorithm-data-structure/data_science/DATASCIENCECLASS/my_covid.csv")

#7.missing value
df<- read.csv("C:/Users/HP/Desktop/algorithm-data-structure/data_science/DATASCIENCECLASS/NAexample.csv")
a<-c(1:5, rep(NA,3),6:10)
a
sum(a)
b<-a[!is.na(a)] 
b
sum(b)

#creating a vector of integers having NAs
a<-c(seq(1,10,3), rep(NA,4), seq(10,2,-2))
a

#assigning 0 to NAs
a[is.na(a)]<-0
a

#assign mean value to NA
a[is.na(a)] <- mean(a, na.rm = TRUE)
a

#is.na(df) #whole dataframe
#is.na(df[“VarA”]) #column

#8.handling missing values
library(readr)
df<- read.csv("C:/Users/HP/Desktop/algorithm-data-structure/data_science/DATASCIENCECLASS/NAexample.csv")
View(df)
df$VarA[is.na(df$VarA)]<-sd(na.omit(df$VarA))
df$VarB[is.na(df$VarB)]<-mean(df$VarB,na.rm=TRUE)
df$VarC[is.na(df$VarC)]<-median(df$VarC,na.rm=TRUE)
View(df)

#9.remove bad values
library(readr)
df_raw <- read.csv("C:/Users/HP/Desktop/algorithm-data-structure/data_science/DATASCIENCECLASS/file1.csv")
df_cleaned <- df_raw
df_logrm <- data.frame(TagsValue=character(), Definition=character(), Value=character())
rows_to_remove <- c()

for (i in 1:nrow(df_raw))
{
  if (df_raw[i, 2] == "Bad value" && df_raw[i, 3] == "Garbage")
  {
    rows_to_remove <- c(rows_to_remove, i)
    # Log the removed bad values
    df_logrm <- rbind(df_logrm, c(df_raw[i, 1], df_raw[i, 2],df_raw[i, 3]))
    # Save the index for the rows to be removed
  }
}
df_cleaned <- df_cleaned[-rows_to_remove, ]
View(df_cleaned)
View (df_logrm)

#9.data transformation
#data transformation (normalization)

#Load Iris Dataset
data(iris)
# View first few rows
head(iris)
# Structure of dataset
str(iris)
#Normalization to Sepal.Length Variable
iris$Sepal.Length_norm <- (iris$Sepal.Length -
                             min(iris$Sepal.Length)) /
  (max(iris$Sepal.Length) - min(iris$Sepal.Length))
head(iris)

#data transformation(standardization)

iris$Sepal.Length
#calculate mean
mean_value <- mean(iris$Sepal.Length)
mean_value
#calculate standard deviation
sd_value <- sd(iris$Sepal.Length)
sd_value
#calculate manually
z_manual <- (iris$Sepal.Length - mean_value) / sd_value
z_manual
#Standardized entire column
iris$Sepal.Length_z_manual <- (iris$Sepal.Length - mean_value) / sd_value
head(iris)
#Standardized using built in function
iris$Sepal.Length_z <- scale(iris$Sepal.Length)
head(iris)

#data transformation(convert)

data(iris)
head(iris)
#check its type
class(iris$Species)
#View values of catagories/level
levels(iris$Species)
#Convert Categorical to numeric
iris$Species_numeric <- as.numeric(iris$Species)
head(iris)
