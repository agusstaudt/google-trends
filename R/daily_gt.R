library(gtrendsR)
library(tidyverse)
library(lubridate)

daily_gt <- function(l, geo="AR", gprop="web", time=c("month","day","week"), 
                         type=c("word","category","topic"), 
                         trial=FALSE, from = 'f', to = 't') {

  
  if(time=="month"){
    
    base_month<-data.frame()
    i=1
    
    if(type=="word"){
    for (x in l) {  
      print(paste(x,i, sep = "-"))
      y <- gtrends(x, geo, gprop, time = paste(from, to))
      if(is.null(y$interest_over_time)){
        next
      } 
      y<-y$interest_over_time[,1:3]
      if(trial==TRUE){
        y$hits[y$hits=="<1"]<-0
        y$hits<-log10(as.numeric(y$hits)+1)
        YearOverYear<-function (x,periodsPerYear){
          if(NROW(x)<=periodsPerYear){
            stop("too few rows")
          }
          else{
            indexes<-1:(NROW(x)-periodsPerYear)
            return(c(rep(NA,periodsPerYear),(x[indexes+periodsPerYear]-x[indexes])))
          }
        }
        y$hits<-YearOverYear(y$hits,12)
        y<-na.omit(y)
        }
      
      base_month<-rbind(base_month, y)
      remove(y, x)
      i=i+1
      }
    }
    
    if(type=="category"){
      g<-data.frame(name=l)
      cat_final<-left_join(g,force(categories),by="name") %>% distinct(name,id)
      id<-cat_final$id
      category<-cat_final$name
      
      for (i in 1:length(id)) {  
        print(paste(category[i],i, sep = "-"))
        y <- gtrends(geo = geo, gprop = gprop, category = id[i], time = time)
        if(is.null(y$interest_over_time)){
          next
        } 
        y<-y$interest_over_time[,1:3]
        if(trial==TRUE){
          y$hits[y$hits=="<1"]<-0
          y$hits<-log10(as.numeric(y$hits)+1)
          YearOverYear<-function (x,periodsPerYear){
            if(NROW(x)<=periodsPerYear){
              stop("too few rows")
            }
            else{
              indexes<-1:(NROW(x)-periodsPerYear)
              return(c(rep(NA,periodsPerYear),(x[indexes+periodsPerYear]-x[indexes])))
            }
          }
          y$hits<-YearOverYear(y$hits,12)
          y<-na.omit(y)}
        
        base_month<-rbind(base_month, y)
        remove(y)
        i=i+1
      }
      remove(category, id) 
    }
  
    base_month <- base_month  %>% group_by(date,keyword) %>% mutate(row = row_number())%>%
      spread(., keyword, hits)%>% select(.,-row) %>% drop_na() %>% ungroup()
    
    assign("base_month", base_month, envir=.GlobalEnv)
    
    colnames(base_month)<- chartr("áéíóúñü", "aeiounu", tolower(colnames(base_month)))
    colnames(base_month)<-str_replace_all(colnames(base_month), pattern=" ", repl="_")  
    
  }

  if(time=="day"){
    
    base_day<-data.frame()
    i=1
        
    if(type=="word"){
          
          for (x in l) {  
            print(paste(x,i, sep = "-"))
            
            m <- gtrends(x, geo = geo, gprop = gprop, time = paste(from, to))
            
            if(is.null(m$interest_over_time)){
              next
            }
            
            m <- m$interest_over_time[,1:3] %>%
              mutate(hits = ifelse(hits=="<1",0, as.numeric(hits))) %>%
              group_by(month = floor_date(date, 'month')) %>%
              summarise(hits = sum(hits)) %>%
              mutate(ym = format(month, '%Y-%m'),
                    hits_norm = hits / max(hits))  %>%
              dplyr::select(ym, hits_norm) %>%
              as.data.frame() %>% ungroup() 
            
            rm <- tibble(s = seq(ymd(from), ymd(to), by = 'month'), 
                        e = seq(ymd(from), ymd(to), by = 'month') + months(1) - days(1))
            
            trends_m <- data.frame()
            
          for (c in seq(1, nrow(rm), 1)) {
              if(as.Date(substr(Sys.time(), 1,10))-rm$e[3]>=0){
            d_trends <- gtrends(x, geo = geo, time = paste(rm$s[c], rm$e[c]))
              print(paste('since', rm$s[c], rm$e[c], 'they were', 
                          count(d_trends$interest_over_time), 'days of gtrends imported'))
              trends_m <- rbind(trends_m,
                              d_trends$interest_over_time[,1:3])} 
            else {d_trends <- gtrends(x, geo = geo, time = paste(rm$s[c],as.Date(substr(Sys.time(), 1,10))))
              print(paste('since', rm$s[c], rm$e[c], 'they were', 
                          count(d_trends$interest_over_time), 'days of gtrends imported'))
                    trends_m <- rbind(trends_m,d_trends$interest_over_time[,1:3])}
                              }
            }
            
            trends_m <- trends_m %>% mutate(ym = format(date, '%Y-%m'),
                                            hits = ifelse(hits=="<1",0, 
                                                          as.numeric(hits))) %>%
              as.data.frame()
            
            y <- trends_m %>%
              left_join(m, by = 'ym') %>%
              mutate(est_hits = as.numeric(hits) * hits_norm) %>%
              select(date, est_hits, keyword) %>%
              as.data.frame() %>%
              mutate(date = as.Date(date))
            
            
            if(trial==TRUE){
              y$est_hits<-log10(as.numeric(y$est_hits)+1)}
            
            base_day<-rbind(base_day, y)
            remove(y, x)
            i=i+1
          }
      
    base_day <- base_day  %>% group_by(date,keyword) %>% mutate(row = row_number())%>%
      spread(., keyword, est_hits)%>% select(.,-row) %>% drop_na() %>% ungroup()
    
    assign("base_day", base_day, envir=.GlobalEnv)
    
    colnames(base_day)<- chartr("áéíóúñü", "aeiounu", tolower(colnames(base_day)))
    colnames(base_day)<-str_replace_all(colnames(base_day), pattern=" ", repl="_")  
  }
}
  



