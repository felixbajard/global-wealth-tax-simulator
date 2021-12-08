type1_error_message <- "Mis-specification: Please select strictly increasing thresholds. The lowest available option here is 100 thousand $, while the highest is 100 billion $. For example, a correct set of increasing thresholds if you chose to have three brackets would be 100k, 100m and 100bn. Rates do not have to be progressive on the other hand."
type2_error_message <- "This graph needs at least two different brackets to be plotted."

data <- read_dta("sample3.dta")
data <- data %>% distinct()

range <- 10^(seq(4.5,12,0.1))

listBrackets <- list(
  "100k US$" = 1e5,
  "200k US$" = 2e5,
  "500k US$" = 5e5,
  "1m US$" = 1e6,
  "2m US$" = 2e6,
  "5m US$" = 5e6,
  "10m US$" = 1e7,
  "20m US$" = 2e7,
  "50m US$" = 5e7,
  "100m US$" = 1e8,
  "200m US$" = 2e8,
  "500m US$" = 5e8,
  "1b US$" = 1e9,
  "2b US$" = 2e9,
  "5b US$" = 5e9,
  "10b US$" = 1e10,
  "20b US$" = 2e10,
  "50b US$" = 5e10,                     
  "100b US$" = 1e11)

listBrackets2 <- list(
  "100 - 200k US$" = 1e5,
  "200 - 500k US$" = 2e5,
  "500k - 1m US$" = 5e5,
  "1 - 2m US$" = 1e6,
  "2 - 5m US$" = 2e6,
  "5 - 10m US$" = 5e6,
  "10 - 20m US$" = 1e7,
  "20 - 50m US$" = 2e7,
  "50 - 100m US$" = 5e7,
  "100 - 200m US$" = 1e8,
  "200 - 500m US$" = 2e8,
  "500m - 1b US$" = 5e8,
  "1 - 2b US$" = 1e9,
  "2 - 5b US$" = 2e9,
  "5 - 10b US$" = 5e9,
  "10 - 20b US$" = 1e10,
  "20 - 50b US$" = 2e10,
  "50 - 100b US$" = 5e10,                     
  "100 - 200b US$" = 1e11)

listISO <- list("World" = "WO",
                "Europe" = "QE",
                "North America" = "XB",
                "East Asia" = "QL",
                "Latin America" = "XL",
                "South & South-East Asia" = "XS",
                "Middle-East & North Africa" = "XN",
                "Sub-Saharan Africa" = "XF",
                "Russia & Central Asia" = "XR")
listISO2 <- c(listISO,"All regions" = "WO QE XB QL XL XS XN XF XR")

effrate <- function(x, rates, thresholds) {
  y <- 0 
  if(x > thresholds[[1]]){
    y <- y + (rates[[1]]/100)*(x-thresholds[[1]])/x
  }
  for(i in 2:length(rates)){
    if(x > thresholds[[i]]){
      y <- y + (rates[[i]]/100)*(x-thresholds[[i]])/x + (rates[[i-1]]/100)*(thresholds[[i]]-x)/x
    }
  }
  return(y)
}

function(input,output,server) {
  
  # toggleModal(session, "startupModal", toggle = "open")
  
  
  output$sample_table <- DT::renderDataTable({
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th('', title = 'Row number'),
          th('iso', title = 'Region code'),
          th('year', title = 'Year'),
          th('threshold', title = 'Threshold'),
          th('threshold_cst', title = 'Constant USD Threshold'),
          th('mnninc999i', title = 'Regional income'),
          th('mhweal999i', title = 'Regional wealth'),
          th('w', title = "Top wealth"),
          th('n', title = "Top number")
        )
      )
    ))
    datatable(
      data[data$iso %in% as.list(strsplit(input$regiondata, " ")[[1]]),][1:10,] %>% select(iso,year,threshold,threshold_cst,mnninc999i,mhweal999i,w,n) %>% mutate(year = as.integer(year),threshold = as.integer(threshold),n = as.integer(round(n))),
      container = sketch
    )
  })
  
  output$downloadcsv <- downloadHandler(
    filename = function(){"raw_data_global_tax_simulator_WID.csv"}, 
    content = function(fname){
      write.csv(subset(data, iso %in% as.list(strsplit(input$regiondata, " ")[[1]])) %>% select(iso,year,threshold,threshold_cst,mnninc999i,mhweal999i,w,n), fname)
    }
  )
  # output$downloadxlsx <- downloadHandler(
  #   filename = function(){"raw_data_global_tax_simulator_WID.xlsx"}, 
  #   content = function(fname){
  #     write.xlsx(subset(data, iso %in% as.list(strsplit(input$regiondata, " ")[[1]])) %>% select(iso,year,threshold,threshold_cst,mnninc999i,mhweal999i,w,n), fname)
  #   }
  # )
  
  output$input_rates_ui <- renderUI({
    nbBrackets <- as.integer(input$nbBrackets)
    
    lapply(1:nbBrackets, function(i) {
      numericInput(paste0("rate_", i), label = paste0("Rate ", i, " (%)"), value = 0.5*i+0.5*max((i-2),0)^2, min = 0.1 , max = 100, step = 0.1)
    })
  })
  
  output$input_thresh_ui <- renderUI({
    nbBrackets <- as.integer(input$nbBrackets)
    
    lapply(1:nbBrackets, function(i) {
      if (i==1)
        selectInput(paste0("threshold_", i), label = paste0("Threshold ", i), choices  = listBrackets, selected = listBrackets[[round(19*i/nbBrackets)]])
      else 
        selectInput(paste0("threshold_", i), label = paste0("Threshold ", i), choices  = listBrackets[listBrackets>listBrackets[[i-1]]], selected = listBrackets[[round(19*i/nbBrackets)]])
    })
  })
  
  output$struct_table <- renderTable({
    
    nbBrackets <- as.integer(input$nbBrackets)
    thresholds <- lapply(1:nbBrackets, function(i) {
      input[[paste0("threshold_", i)]]
    })

    
    if (nbBrackets>0){}
    rates <- lapply(1:nbBrackets, function(i) {
      input[[paste0("rate_", i)]]
    })
    rates <- do.call(rbind.data.frame, rates)
    
    
    if (length(names(rates)[1])>0){
      validate(
        need(all(diff(as.numeric(thresholds))>0), type1_error_message)
      )
      
      names(rates)[1] <- 'rate'
    

    intermediary <- data[data$threshold %in% thresholds & data$iso==input$region & data$year==2021,] %>% cbind(rates)  %>% arrange(desc(threshold)) %>% mutate(
      wb = c(w[1], (w - lag(w))[-1]),
      nb = c(n[1], (n - lag(n))[-1]),
      revenue = ((wb-nb*threshold_cst)*rate/mnninc999i)
    )  %>% arrange(threshold) %>% mutate(
      tb1 = c(0, (threshold_cst - lag(threshold_cst))[-1]*lag(rate)[-1]),
      tb2 = c(0, lag(tb1)[-1]),
      tb3 = c(0, lag(tb2)[-1]),
      tb4 = c(0, lag(tb3)[-1]),
      tb5 = c(0, lag(tb4)[-1]),
      tb6 = c(0, lag(tb5)[-1]),
      tb7 = c(0, lag(tb6)[-1]),
      revenue = revenue + (tb1+tb2+tb3+tb4+tb5+tb6+tb7)*nb/mnninc999i, 
      revenue = revenue*(1-input$evasion/100)*(1-input$depreciation/100)
    )  %>% select(group,threshold_,rate,n,w,nb,wb,revenue,tb1,tb2,tb3,tb4,tb5,tb6,tb7,threshold_cst,mnninc999i) 
    
    
    intermediary  %>% summarise(across(c("nb","wb","revenue"), sum)) %>% bind_rows(intermediary) %>% 
      fill(mnninc999i, .direction = "up") %>% mutate(effrate = revenue*mnninc999i/wb) %>% 
      mutate(bracket = ifelse(is.na(group),paste("All above ",intermediary$group[1]),ifelse(!is.na(lead(group)),paste(group,lead(group),sep="-"),paste("Above ",group))))  %>% 
      select(bracket, rate, wb, nb, revenue, effrate) %>% mutate(rate = round(rate,1), wb = as.integer(round(wb/1e9)), nb=as.integer(round(signif(nb,4)))) %>% 
      rename("Wealth group" = bracket, "Rate (%)" = rate, "Total wealth ($ bn)" = wb, "Number of adults" = nb, "Revenues (% of regional income)" = revenue, "Effective wealth tax rate (%)" = effrate)
    }
  }, width = "auto")  
  
  
  output$eff_rate <- renderPlot({
    
    nbBrackets <- as.integer(input$nbBrackets)
    thresholds <- as.numeric(lapply(1:nbBrackets, function(i) {
      input[[paste0("threshold_", i)]]
    }))
    rates <- as.numeric(lapply(1:nbBrackets, function(i) {
      input[[paste0("rate_", i)]]
    }))
    validate(
      need(all(diff(as.numeric(thresholds))>0), type1_error_message),
      need(nbBrackets>1, type2_error_message)
    )
    
    if (nbBrackets>1) {
    effrates <- sapply(range,effrate,rates=rates,thresholds=thresholds)
    ploteffrates <- data.frame(range,effrates) %>% mutate(effrates = effrates*100)
    ggplot(ploteffrates,aes(x=range,y=effrates))+geom_line()+scale_x_continuous(trans='log2', breaks = c(1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11), labels = c("100k", "1m", "10m", "100m", "1b", "10b", "100b")) + ylab("Effective wealth tax rate (%)") + xlab("Individual wealth ($)") + theme(
      legend.position = c(0.12, 0.9),
      axis.title.x = element_text(face="bold"),
      axis.title.y = element_text(face="bold"),
      plot.background = element_rect(fill = "white", colour = "white"),
      panel.background = element_rect(fill = "#E6EFF2", colour = "white"),
      axis.text = element_text(colour = "#0b6685"),
      axis.title = element_text(colour = "#0b6685")
    )
    }
    
  })
  
  
  output$comparison <- renderPlot({
    
    nbBrackets <- as.integer(input$nbBrackets)
    thresholds <- lapply(1:nbBrackets, function(i) {
      as.numeric(input[[paste0("threshold_", i)]])
    })
    rates <- lapply(1:nbBrackets, function(i) {
      input[[paste0("rate_", i)]]
    })
    validate(
      need(all(diff(as.numeric(thresholds))>0), type1_error_message)
    )
    rates <- do.call(rbind.data.frame, rates) %>% cbind(do.call(rbind.data.frame, thresholds))
    names(rates) <- c('rate','threshold')
    
    threshold <- as.numeric(input$t_comparison)
    
    
    
    diffw <- data[data$iso==input$region,]  %>% group_by(group)  %>% arrange(year) %>% select(group, year, wb, nb,mhweal999i) %>% mutate(diffw = wb/lag(wb)-1, default=0) %>% ungroup() %>% arrange(group, year) %>% select(group,year,diffw,nb,wb,mhweal999i) %>% rename(wb0 = wb) %>% pivot_wider(names_from = year, values_from = c(diffw,nb,wb0,mhweal999i))
    diffw[is.na(diffw)] <- 0
    
    data2 <- data[data$iso==input$region & data$year == 2010,]  %>% group_by(year) %>% arrange(group, by_group = TRUE) %>% merge(rates, all.x=T) %>% fill(rate) %>% replace_na(list(rate=0)) %>% select(year,group,w,n,wb,nb,rate,threshold_cst,threshold) %>% mutate(rate = rate/100, revenue = (wb-nb*threshold_cst)*rate) %>% mutate(
      tb1 = c(0, (threshold_cst - lag(threshold_cst))[-1]*lag(rate)[-1]),
      tb2 = c(0, lag(tb1)[-1]),
      tb3 = c(0, lag(tb2)[-1]),
      tb4 = c(0, lag(tb3)[-1]),
      tb5 = c(0, lag(tb4)[-1]),
      tb6 = c(0, lag(tb5)[-1]),
      tb7 = c(0, lag(tb6)[-1]),
      tb8 = c(0, lag(tb7)[-1]),
      tb9 = c(0, lag(tb8)[-1]),
      tb10 = c(0, lag(tb9)[-1]),
      tb11 = c(0, lag(tb10)[-1]),
      tb12 = c(0, lag(tb11)[-1]),
      tb13 = c(0, lag(tb12)[-1]),
      tb14 = c(0, lag(tb13)[-1]),
      tb15 = c(0, lag(tb14)[-1]),
      tb16 = c(0, lag(tb15)[-1]),
      tb17 = c(0, lag(tb16)[-1]),
      tb18 = c(0, lag(tb17)[-1]),
      tb19 = c(0, lag(tb18)[-1]),
      revenue = revenue + (tb1+tb2+tb3+tb4+tb5+tb6+tb7+tb8+tb9+tb10+tb11+tb12+tb13+tb14+tb15+tb16+tb17+tb18+tb19)*nb,
      revenue = revenue*(1-input$evasion/100)*(1-input$depreciation/100)) %>% select(group, threshold_cst,rate,wb,revenue,tb1,tb2,tb3,tb4,tb5,tb6,tb7,tb8,tb9,tb10,tb11,tb12,tb13,tb14,tb15,tb16,tb17,tb18,tb19,threshold) %>% merge (diffw, all.x = T) %>% arrange(threshold_cst) %>% rename(wb_2010 = wb, rev_2010 = revenue)
    
    for(y in 2011:2021){
      dynamicdiffw <- paste0("diffw_", y)
      dynamicwb <- paste0("wb_", y)
      dynamicwb0 <- paste0("wb0_", y)
      dynamicwbm <- paste0("wb_", y-1)
      dynamicr <- paste0("rev_", y)
      dynamicrm <- paste0("rev_", y-1)
      dynamicn <- paste0("nb_", y)
      
      data2[, dynamicwb] <- (data2[, dynamicwbm] - data2[, dynamicrm])*(1+data2[, dynamicdiffw])
      for(g in c("100b","50b","20b","10b","5b","2b","1b","500m","200m","100m")){
        if (is.na(data2[data2$group==g,dynamicwb])){
          data2[data2$group==g,dynamicwb] <-  data2[data2$group==g,dynamicwb0]
        }
      }
      
      data2[, dynamicr]  <- ((data2[, dynamicwb] - data2[, dynamicn]*data2[, "threshold_cst"])*data2[, "rate"] + data2[,dynamicn]*(data2[,"tb1"]+data2[,"tb2"]+data2[,"tb3"]+data2[,"tb4"]+data2[,"tb5"]+data2[,"tb6"]+data2[,"tb7"]+data2[,"tb8"]+data2[,"tb9"]+data2[,"tb10"]+data2[,"tb11"]+data2[,"tb12"]+data2[,"tb13"]+data2[,"tb14"]+data2[,"tb15"]+data2[,"tb16"]+data2[,"tb17"]+data2[,"tb18"]+data2[,"tb19"]))*(1-input$evasion/100)*(1-input$depreciation/100)
    }
    
    data2
    
    data2 <- data2 %>% pivot_longer(cols=c(starts_with("wb_"),starts_with("diffw_"),starts_with("rev_"),starts_with("nb_"),starts_with("wb0_"),starts_with("mhweal999i_")), names_to = c(".value","year"),names_sep = "_")  %>% select(year,group,threshold_cst,wb0,wb,mhweal999i,threshold)  %>% group_by(year) %>% arrange(desc(threshold_cst),by_group = F) %>% mutate(w0 = cumsum(wb0), w = cumsum(wb), mhweal999i_new = mhweal999i + last(w)-last(w0), w0 = w0/mhweal999i*100, w=w/mhweal999i_new*100)
    
    data2 <- data2[round(data2$threshold) == round(threshold),] %>% select(year,w0,w) %>% pivot_longer(cols=c(w,w0)) %>% ungroup(year) %>% group_by(name) 
    data2$year <- as.integer(data2$year)
    data2
    
    graph <- ggplot() + geom_line(data=data2, aes(x=year, y = value, group = name, col = name)) + geom_point() + scale_linetype_manual(values=c("twodash", "dotted","dotted")) + ylab("Top share of total wealth (%)") + xlab("Year") + labs(color = "Scenario") +
       scale_color_manual(labels = c("Tax implemented", "No tax (actual levels)"), values = c("#053242", "#3B849D")) + theme(
         legend.position = c(0.12, 0.9),
         axis.title.x = element_text(face="bold"),
         axis.title.y = element_text(face="bold"),
         plot.background = element_rect(fill = "white", colour = "white"),
         panel.background = element_rect(fill = "#E6EFF2", colour = "white"),
         axis.text = element_text(colour = "#0b6685"),
         axis.title = element_text(colour = "#0b6685")
       )  + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
    graph
    

  })
  
  output$comparison2 <- renderPlot({
    
    nbBrackets <- as.integer(input$nbBrackets)
    thresholds <- lapply(1:nbBrackets, function(i) {
      as.numeric(input[[paste0("threshold_", i)]])
    })
    rates <- lapply(1:nbBrackets, function(i) {
      input[[paste0("rate_", i)]]
    })
    validate(
      need(all(diff(as.numeric(thresholds))>0), type1_error_message)
    )
    rates <- do.call(rbind.data.frame, rates) %>% cbind(do.call(rbind.data.frame, thresholds))
    names(rates) <- c('rate','threshold')
    
    threshold <- as.numeric(input$t_comparison2)
    
    
    
    diffw <- data[data$iso==input$region,]  %>% group_by(group)  %>% arrange(year) %>% select(group, year, wb, nb,mhweal999i) %>% mutate(diffw = wb/lag(wb)-1, default=0) %>% ungroup() %>% arrange(group, year) %>% select(group,year,diffw,nb,wb,mhweal999i) %>% rename(wb0 = wb) %>% pivot_wider(names_from = year, values_from = c(diffw,nb,wb0,mhweal999i))
    diffw[is.na(diffw)] <- 0
    
    data2 <- data[data$iso==input$region & data$year == 2010,]  %>% group_by(year) %>% arrange(group, by_group = TRUE) %>% merge(rates, all.x=T) %>% fill(rate) %>% replace_na(list(rate=0)) %>% select(year,group,w,n,wb,nb,rate,threshold_cst,threshold) %>% mutate(rate = rate/100, revenue = (wb-nb*threshold_cst)*rate) %>% mutate(
      tb1 = c(0, (threshold_cst - lag(threshold_cst))[-1]*lag(rate)[-1]),
      tb2 = c(0, lag(tb1)[-1]),
      tb3 = c(0, lag(tb2)[-1]),
      tb4 = c(0, lag(tb3)[-1]),
      tb5 = c(0, lag(tb4)[-1]),
      tb6 = c(0, lag(tb5)[-1]),
      tb7 = c(0, lag(tb6)[-1]),
      tb8 = c(0, lag(tb7)[-1]),
      tb9 = c(0, lag(tb8)[-1]),
      tb10 = c(0, lag(tb9)[-1]),
      tb11 = c(0, lag(tb10)[-1]),
      tb12 = c(0, lag(tb11)[-1]),
      tb13 = c(0, lag(tb12)[-1]),
      tb14 = c(0, lag(tb13)[-1]),
      tb15 = c(0, lag(tb14)[-1]),
      tb16 = c(0, lag(tb15)[-1]),
      tb17 = c(0, lag(tb16)[-1]),
      tb18 = c(0, lag(tb17)[-1]),
      tb19 = c(0, lag(tb18)[-1]),
      revenue = revenue + (tb1+tb2+tb3+tb4+tb5+tb6+tb7+tb8+tb9+tb10+tb11+tb12+tb13+tb14+tb15+tb16+tb17+tb18+tb19)*nb,
      revenue = revenue*(1-input$evasion/100)*(1-input$depreciation/100)) %>% select(group, threshold_cst,rate,wb,revenue,tb1,tb2,tb3,tb4,tb5,tb6,tb7,tb8,tb9,tb10,tb11,tb12,tb13,tb14,tb15,tb16,tb17,tb18,tb19,threshold) %>% merge (diffw, all.x = T) %>% arrange(threshold_cst) %>% rename(wb_2010 = wb, rev_2010 = revenue)
    
    for(y in 2011:2021){
      dynamicdiffw <- paste0("diffw_", y)
      dynamicwb <- paste0("wb_", y)
      dynamicwb0 <- paste0("wb0_", y)
      dynamicwbm <- paste0("wb_", y-1)
      dynamicr <- paste0("rev_", y)
      dynamicrm <- paste0("rev_", y-1)
      dynamicn <- paste0("nb_", y)
      
      data2[, dynamicwb] <- (data2[, dynamicwbm] - data2[, dynamicrm])*(1+data2[, dynamicdiffw])
      for(g in c("100b","50b","20b","10b","5b","2b","1b","500m","200m","100m")){
        if (is.na(data2[data2$group==g,dynamicwb])){
          data2[data2$group==g,dynamicwb] <-  data2[data2$group==g,dynamicwb0]
        }
      }
      
      data2[, dynamicr]  <- ((data2[, dynamicwb] - data2[, dynamicn]*data2[, "threshold_cst"])*data2[, "rate"] + data2[,dynamicn]*(data2[,"tb1"]+data2[,"tb2"]+data2[,"tb3"]+data2[,"tb4"]+data2[,"tb5"]+data2[,"tb6"]+data2[,"tb7"]+data2[,"tb8"]+data2[,"tb9"]+data2[,"tb10"]+data2[,"tb11"]+data2[,"tb12"]+data2[,"tb13"]+data2[,"tb14"]+data2[,"tb15"]+data2[,"tb16"]+data2[,"tb17"]+data2[,"tb18"]+data2[,"tb19"]))*(1-input$evasion/100)*(1-input$depreciation/100)
    }
    
    data2
    
    data2 <- data2 %>% pivot_longer(cols=c(starts_with("wb_"),starts_with("diffw_"),starts_with("rev_"),starts_with("nb_"),starts_with("wb0_"),starts_with("mhweal999i_")), names_to = c(".value","year"),names_sep = "_")  %>% select(year,group,threshold_cst,wb0,wb,mhweal999i,threshold)  %>% group_by(year) %>% arrange(desc(threshold_cst),by_group = F) %>% mutate(w0 = cumsum(wb0), w = cumsum(wb), mhweal999i_new = mhweal999i + last(w)-last(w0), wb0 = wb0/mhweal999i*100, wb=wb/mhweal999i_new*100)
    
    data2 <- data2[round(data2$threshold) == round(threshold),] %>% select(year,wb0,wb) %>% pivot_longer(cols=c(wb,wb0)) %>% ungroup(year) %>% group_by(name) 
    data2$year <- as.integer(data2$year)
    data2
    
    graph <- ggplot() + geom_line(data=data2, aes(x=year, y = value, group = name, col = name)) + geom_point() + scale_linetype_manual(values=c("twodash", "dotted","dotted")) + ylab("Share of total wealth (%)") + xlab("Year") + labs(color = "Scenario") +
      scale_color_manual(labels = c("Tax implemented", "No tax (actual levels)"), values = c("#053242", "#3B849D")) + theme(
        legend.position = c(0.12, 0.9),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "#E6EFF2", colour = "white"),
        axis.text = element_text(colour = "#0b6685"),
        axis.title = element_text(colour = "#0b6685")
      )  + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
    graph
    
    
  })
}

