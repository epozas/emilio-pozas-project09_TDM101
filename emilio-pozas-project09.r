benfords_law <- function(d) log10(1+1/d)

digits <-1:9
bf_val<-benfords_law(digits)
plot(type="l",digits, bf_val, xlab = "digits", ylab="probabilities", main="Benfords Law Plot Line")

first_digit <- function (n) {
    n <- abs(n)
    if(n != 0) {
        while(n >= 10) {
            n <- n/10
        }
        while(n <1) {
            n <- n*10
         }
        n <- floor(n)
    }
    
    return(n)
}
first_digit(403434)
first_digit(000.000303434)

library(data.table)
myDF <- fread("/anvil/projects/tdm/data/restaurant/orders.csv")
head(myDF)
myDF$fd_grand_total <- sapply(myDF$grand_total, first_digit)

table(myDF$fd_grand_total)
plot(type="l",table(myDF$fd_grand_total)/ length(myDF$fd_grand_total), xlab = "digits", ylab="probabilities", main="Grand_Total First digit plot")

as.Date(head(myDF$delivery_date))
find_orders <- function(start_date, end_date) {
    orders_by_dates <- subset (myDF,(( myDF$delivery_date >= start_date) & (myDF$delivery_date <= end_date)))
    return(orders_by_dates)
    }
myresulstDF<-find_orders("2019-05-21","2020-11-21")
dim(myresulstDF)
head(myresulstDF)