#Install "dplyr" and "stringr" packages and use the library
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyr")
library("dplyr")
library("stringr")
library("tidyr")

# Load the data from companies and rounds2 file.
companies <- read.delim("companies.txt", header =  TRUE , stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)

# Load the mapping file
mapping <- read.csv("mapping.csv", header =  TRUE, stringsAsFactors = FALSE)


#clean(convert to lower case) the permalink column in companies and rounds2 dataframe
companies <- mutate(companies, permalink1 = str_to_lower(companies$permalink))
companies <- companies[,-1]
rounds2 <- mutate(rounds2, permalink1 = str_to_lower(rounds2$company_permalink))
rounds2 <- rounds2[,-1]

# Find the distinct number of companies, permalink1 is the unique identifier for the company
NROW(unique(companies$permalink))
NROW(unique(rounds2$permalink))

# Are there any companies in the rounds2 file which are not present in companies? 
setdiff(rounds2$permalink1, companies$permalink1)

#merge companies and rounds2
master_frame <- left_join(rounds2, companies, by = "permalink1")

#Grouping on Fund type, and calculating mean(raised_amount_type)
master_fund_group <- group_by(master_frame, funding_round_type)
fund_investment <- summarise(master_fund_group, mean(raised_amount_usd, na.rm = TRUE))
colnames(fund_investment) <- c("fund_type", "avg_raised_amt")

#Group by country, and calculating mean(raised_amount_type) for Venture funding type.
master_country_group <- filter(group_by(master_frame, country_code), funding_round_type == "venture")
top9 <- summarise(master_country_group, sum(raised_amount_usd, na.rm = TRUE))
colnames(top9) <- c("Country_Code","Total_Sum")
top9 <- head(arrange(top9, desc(Total_Sum)),9)

# Checkpoint 4 - Sector Analysis 1

master_frame_1 <- separate(master_frame, category_list, into = c("primary_sector","sub_sectors1"), sep ="\\|")  
maps <- gather(mapping, main_sector, Value, Automotive...Sports: Social..Finance..Analytics..Advertising)
maps <- maps[!(maps$Value ==0),]
maps$Value <- NULL
master_frame_2 <- merge(master_frame_1, maps, by.x="primary_sector", by.y= "category_list", all.x = TRUE)

# Checkpoint 5 - Sector Analysis 2

# For Country : USA - FT : Venture - Investment : 5M - 15M
D1 <- filter(master_frame_2, master_frame_2$funding_round_type == "venture",master_frame_2$country_code == "USA", master_frame_2$raised_amount_usd <= 15000000, master_frame_2$raised_amount_usd >=5000000) 
sum_D1 <- sum(D1$raised_amount_usd)
c_D1 <- length(D1$raised_amount_usd)
group_D1 <- group_by(D1, main_sector)
D1_count <- summarise(group_D1, n(), sum(raised_amount_usd))
D1 <- merge(D1,D1_count,by="main_sector")

# For Country : GBR - FT : Venture - Investment : 5M - 15M
D2 <- filter(master_frame_2, master_frame_2$funding_round_type == "venture",master_frame_2$country_code == "GBR", master_frame_2$raised_amount_usd <= 15000000, master_frame_2$raised_amount_usd >=5000000) 
sum_D2 <- sum(D2$raised_amount_usd)
c_D2 <- length(D2$raised_amount_usd)
group_D2 <- group_by(D2, main_sector)
D2_count <- summarise(group_D2, n(), sum(raised_amount_usd))
D2 <- merge(D2,D2_count,by="main_sector")

# For Country : IND - FT : Venture - Investment : 5M - 15M
D3 <- filter(master_frame_2, master_frame_2$funding_round_type == "venture",master_frame_2$country_code == "IND", master_frame_2$raised_amount_usd <= 15000000, master_frame_2$raised_amount_usd >=5000000) 
sum_D3 <- sum(D3$raised_amount_usd)
c_D3 <- length(D3$raised_amount_usd)
group_D3 <- group_by(D3, main_sector)
D3_count <- summarise(group_D3, n(), sum(raised_amount_usd))
D3 <- merge(D3,D3_count,by="main_sector")

# For the top sector count-wise (point 3), which company received the highest investment?

# For USA
D1_top <- filter(D1, D1$main_sector =="Others")
top_d1 <- group_by(D1_top, name)
summary_D1 <- summarise(top_d1, n(), sum(raised_amount_usd))

# For GBR
D2_top <- filter(D2, D2$main_sector =="Others")
top_d2 <- group_by(D2_top, name)
summary_D2 <- summarise(top_d2, n(), sum(raised_amount_usd))

# For IND
D3_top <- filter(D3, D3$main_sector =="Others")
top_d3 <- group_by(D3_top, name)
summary_D3 <- summarise(top_d3, n(), sum(raised_amount_usd))


# For the second best sector count-wise (point 4), which company received the highest investment?

# For USA
D1_second <- filter(D1, D1$main_sector =="Cleantech...Semiconductors")
Second_D1 <- group_by(D1_second, name)
summary2_D1 <- summarise(Second_D1, n(), sum(raised_amount_usd))

# For GBR
D2_second <- filter(D2, D2$main_sector =="Cleantech...Semiconductors")
Second_D2 <- group_by(D2_second, name)
summary2_D2 <- summarise(Second_D2, n(), sum(raised_amount_usd))

# For IND
D3_second <- filter(D3, D3$main_sector =="News..Search.and.Messaging")
Second_D3 <- group_by(D3_second,str_to name)
summary2_D3 <- summarise(Second_D3, n(), sum(raised_amount_usd))
