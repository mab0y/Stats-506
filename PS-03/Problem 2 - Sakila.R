library(DBI) 
sakila <- dbConnect(RSQLite::SQLite(), "~/sakila_master.db")

dbGetQuery(sakila, "SELECT COUNT(1) FROM film ")
dbGetQuery(sakila,"SELECT l.name, COUNT(f.film_id) FROM film f JOIN language l ON f.language_id = l.language_id GROUP BY l.name")

film_category<-dbGetQuery(sakila,"SELECT * FROM film_category")
category<-dbGetQuery(sakila,"SELECT * FROM category")

max_category_id<-sort(table(film_category$category_id),decreasing = TRUE)
max_category_name<-cbind(category$name[as.numeric(names(max_category_id))],as.vector(max_category_id))

dbGetQuery(sakila, "SELECT c.name,COUNT(fc.film_id) as number FROM film_category fc JOIN category c ON fc.category_id = c.category_id GROUP BY c.name ORDER BY number DESC")

# https://www.statmethods.net/management/merging.html
customer<-dbGetQuery(sakila,"SELECT * FROM customer")
address<-dbGetQuery(sakila,"SELECT * FROM address")
city<-dbGetQuery(sakila,"SELECT * FROM city")
country<-dbGetQuery(sakila,"SELECT * FROM country")

cust_address<-merge(customer,address,by="address_id")
cust_address_city<-merge(cust_address,city,by="city_id")
cust_address_city_country<-merge(cust_address_city,country,by="country_id")

country_table<-table(cust_address_city_country$country)
country_name <- country_table[country_table == 9]

dbGetQuery(sakila, "SELECT country.country,COUNT(cust.customer_id) as number FROM customer cust JOIN address a ON cust.address_id=a.address_id JOIN city ON a.city_id=city.city_id JOIN country ON city.country_id=country.country_id GROUP BY country.country HAVING number=9")
