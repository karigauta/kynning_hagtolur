########## Deflation function #####################
# A function that uses Icelandic CPI index to deflate a given set of values
# 
# years = seq(2007,2019)
# value <- test1
# base = 2020

DeflateValues <-  function ( value, years, base ) {
  # value is a vector of values to be deflated
  # years is a vector of correpsponding years
  # base is the year to be used for deflation

  # Check if base is in json query

  
  # thetta saekir gognin fra hagstofu
  pxq <- pxweb_query("C:/Users/kari/github_rep/Baendasamtok/Baendasamtok/json_queries/yearly_CPI.json")
  
  CPI_yearly <- pxweb_get("https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/visitolur/1_vnv/1_vnv/VIS01005.px",
                          pxq)
  CPI_yearly <- as.data.frame(CPI_yearly)
  colnames(CPI_yearly) <-  c("Year", "Type", "Base_year", "Value")
  # Deflator factor 

  deflator_old <- CPI_yearly$Value[match(years, CPI_yearly$Year)]
  # deflator_old <-  CPI_yearly %>% group_by("Year") %>% filter ( Year %in% years ) %>% pull("Value")
  deflator_new <-  CPI_yearly %>% group_by("Year") %>% filter ( Year == base) %>% pull("Value")
    
  deflator_factor <- deflator_new/deflator_old
  
  value <- value*deflator_factor
return(value)
  }
