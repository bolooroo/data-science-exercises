#1
original<-read.csv("refine_original.csv")
head(original, n=16)
table(original$company)

original$company<-tolower(original$company)
table(original$company)

original$company<-sub(" ", "", original$company)
original$company<-sub("0", "o", original$company, fixed = TRUE)
original$company<-sub("f", "ph", original$company, fixed = TRUE)
original$company<-gsub("^(ph)([\\a-z\\-]+)(ps)$", "philips", original$company)
original$company<-sub("unilver", "unilever", original$company, fixed = TRUE)
original$company<-sub("vanhouten", "van houten", original$company, fixed = TRUE)
table(original$company)

#2
str(original)
original$Product.code...number<-as.character(original$Product.code...number)
org_split<-strsplit(original$Product.code...number, split = "-", fixed=TRUE)
head(org_split)

select_el <- function(x, index) {
  x[index]
}
original$product_code<-unlist(lapply(org_split, select_el, index=1))
original$product_number<-unlist(lapply(org_split, select_el, index=2))
original<-original %>% select (-Product.code...number)

#3
table(original$product_category)
original$product_category<-original$product_code
original$product_category<-gsub("p", "Smartphone", original$product_category, fixed = TRUE)
original$product_category<-gsub("v", "TV", original$product_category, fixed = TRUE)
original$product_category<-gsub("x", "Laptop", original$product_category, fixed = TRUE)
original$product_category<-gsub("q", "Tablet", original$product_category, fixed = TRUE)

#4
str(original)
original$full_address<-paste(original$address, original$city, original$country, sep=",")

#5
summary(original)
original<-mutate(original, company_philips = ifelse(original$company == "philips", 1, 0))
original<-mutate(original, company_akzo = ifelse(original$company == "akzo", 1, 0))
original<-mutate(original, company_van_houten = ifelse(original$company == "van houten", 1, 0))
original<-mutate(original, company_unilever = ifelse(original$company == "unilever", 1, 0))
original<-mutate(original, product_smartphone = ifelse(original$product_category == "Smartphone", 1, 0))
original<-mutate(original, product_tv = ifelse(original$product_category == "TV", 1, 0))
original<-mutate(original, product_laptop = ifelse(original$product_category == "Laptop", 1, 0))
original<-mutate(original, product_tablet = ifelse(original$product_category == "Tablet", 1, 0))

#6

write.csv(original, "refine_clean.csv")
           
