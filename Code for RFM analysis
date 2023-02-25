library(tidyverse)

df_salesData <- read.csv("storeData.csv")

date_today <- lubridate::ymd("20111230")

df_salesData |>
  dplyr::mutate(subtotal = Quantity*UnitPrice,
                Bill_date = strptime(InvoiceDate,format = "%m/%d/%Y %H:%M"))|>
  dplyr::group_by(CustomerID)|>
  dplyr::summarise(Total_revenue = sum(subtotal),
                   n_transactions = dplyr::n_distinct(InvoiceNo),
                   last_purchase_date = max(Bill_date))|>
  dplyr::mutate(n_days_lastPurchase =
                  difftime(date_today,last_purchase_date,units = ("days")))|> 
  dplyr::mutate(n_days_lastPurchase = as.integer(n_days_lastPurchase)) |>
  janitor::clean_names() |>  
  dplyr::select(customer_id, total_revenue, n_transactions, 
                n_days_last_purchase) |> 
  drop_na() -> df_rfm_1

df_rfm_1 |> 
  dplyr::mutate(monetary_values = Hmisc::cut2(total_revenue, g = 5),
                recency_values = Hmisc::cut2(n_days_last_purchase, g = 5),
                freq_values = Hmisc::cut2(n_transactions, g = 5)) -> df_rfm_2
df_rfm_2 |> 
  dplyr::mutate(monetary_score = as.integer(monetary_values),
                recency_score = as.integer(recency_values),
                freq_score = as.integer(freq_values),
                recency_score = dense_rank(desc(recency_score))) -> df_rfm_3
df_rfm_3 |> 
  dplyr::mutate(Types = ifelse(recency_score >= 4 & (freq_score + monetary_score)/2 >= 4,"Champions",
                                ifelse(recency_score >= 2 & (freq_score + monetary_score)/2 >= 3, "Loyal Customers",
                                       ifelse(recency_score >=3 & (freq_score + monetary_score)/2 >1, "Potential Loyalists", 
                                              ifelse(recency_score >= 4 & (freq_score + monetary_score)/2 ==1 , "Recent_customers",
                                                     ifelse((recency_score ==3 & recency_score ==4)  & (freq_score + monetary_score)/2 == 1 , "Promising",
                                                            ifelse( recency_score <=3 & (freq_score + monetary_score)/2 ==2 & (freq_score + monetary_score)/2 ==3,"customers needing attention", 
                                                                   ifelse(recency_score ==2 &  (freq_score + monetary_score)/2 <=2, "About to sleep",
                                                                          ifelse(recency_score <=2 & (freq_score + monetary_score)/2 <=4, "At Risk",
                                                                                 ifelse(recency_score == 1 & freq_score >=4 & monetary_score >=4, "Can't Lose Them",
                                                                                        ifelse(recency_score ==1 & freq_score <= 2 & monetary_score <=2, "Hibernating",
                                                                                               ifelse(recency_score <=2 & (freq_score + monetary_score)/2 <=2, "Lost",
                                                                                                      ifelse(recency_score ==1 & freq_score <= 3 & monetary_score ==5, "Can't Lose Them",
                                                                                                             ifelse(recency_score ==3 & (freq_score + monetary_score)/2 ==1, "New customers", "Others")))))))))))))) -> df_rfm_3

library(tidyverse)
library(plotly)
ggplotly(
  ggplot(df_rfm_3, aes(Types,fill = Types))
  +ggtitle("RFM Customer Segmentation Graph") 
  +geom_bar()
  +theme(axis.text.x =  element_text(angle = 100))
  +theme(title = element_text(face = 'bold', size = 12))
)




       



install.packages("plotrix")

x <- table(df_rfm_3$Types)
class(df_rfm_3$Types)
piepercent <- round(100*x/sum(x),1)
lbls = paste(names(x),"",piepercent,"%")
library(plotrix)

pie_segmentation <- pie3D(x,explode = 0.1, radius = 1, 
                            main = "Pie Chart 3D Customer Segmentation")
pie3D.labels(radialpos = pie_segmentation, labels = lbls,
             labelcex = 1,radius = 1.7,theta = pi/5,minsep = .1)
  

 
