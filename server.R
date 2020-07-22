# setup ----

# 1. packages

library(deSolve)
library(plyr)
library(grid)
library(compiler)
library(shinyTime)
library(lubridate)
library(TeachingDemos)
library(rmarkdown)
library(knitr)
library(DT)
library(rsconnect)
library(tidyverse)

# 2. functions

calculate_clearance <- function(POD){
  TVCL <- 20 * (1 + 0.01 * (POD - 10)) # L/h
}

## ltv2mat copy right:: Prof. Bae 

cmat=function(vec){
  LENGTH=length(vec)
  DIM=as.integer(round((sqrt(8*LENGTH+1)-1)/2,0))
  if(DIM*(DIM+1)/2!=LENGTH) return(NULL)
  mat=matrix(nrow=DIM, ncol=DIM)
  mat[upper.tri(mat, diag=TRUE)]=vec
  mat[lower.tri(mat)] = t(mat)[lower.tri(mat)]
  return(mat)
}

# 2. constants

default_dose_example_csv <- '"Date","Dosing_Time","Route","Dose"
"19.03.03","09:00","PO","5"
"19.03.03","21:00","PO","5"
"19.03.04","09:00","PO","5"
"19.03.04","21:00","PO","5"'

TVV1 <- 205 # L
TVKA <-   3.2 # /h

# Eta (Omega)
ETA1SD <- 0.17     # Vancomycin Clearance eta 
ETA2SD <- 0.35     # Vancomycin Volume eta
omega <- cmat(c(ETA1SD ,0, ETA2SD)) %>%  # Prof. Bae's function
  print() 
omega.inv <- solve(omega) %>% 
  print()

# Eps (Sigma)
EPS1SD <- 3.75           # Vancomycin Additive residual error   
EPS2SD <- 0.1          # Vancomycin Proportional residual error
EPS2SDsq <- (EPS2SD)^2 # Vancomycin square

input <- list( # These values should be defined in ui.R.
  obsc = 1000, # ng/mL 
  obsDate = "2019-03-05", 
  obsTime = strptime("10:30", "%R"),
  post_op_date = 3,
  newdose = 1000,
  newtau = 48,
  newinf = 1,
  ll = 15,
  ul = 40,
  age = 40
)

# main ----

shiny::shinyServer(function(input, output) {
  
  # downloadData ----
  
  output$downloadData <- downloadHandler(
    filename <- function() { paste("dose_example", '.csv', sep='') },
    content <- function(file) {
      write_csv(datasetInput(), file)
    }
  )
  
  # dosing_history_contents ----
  output$dosing_history_contents <- renderTable({
    if (is.null(input$file1)) return(read_csv(default_dose_example_csv,
                                              col_types = 'cccd'))
    return(read_csv(input$file1$datapath,
                    col_types = 'cccd'))
  })
  
  # creatinine_clearance ----
  
  output$typical_values <- renderTable({
    # Typical Values of Population PK parameters
    TVCL <- calculate_clearance(input$post_op_date) %>% 
      round(digits = 2)
    return(tibble(`CL/F (L/h)` = TVCL, `V/F (L)` = TVV1, `Ka (/hr)` = TVKA))
  })
  
  # output_table1_time_predicted_concentration ----
  output$output_table1_time_predicted_concentration <- renderTable({
    sim_data_output <- sim.data()
    prtx_predicted_concentration <- sim_data_output$table1
    return(prtx_predicted_concentration)
  })
  
  # outputtable2 ----
  output$outputtable2 <- renderTable({
    sim_data_output <- sim.data()
    prtx_predicted_concentration <- sim_data_output$table2
    return(prtx_predicted_concentration)
  })
  
  # # outputtable3 ----
  # output$outputtable3 <- renderTable({
  #   prt1=sim.data()
  #   prt2=prt1[complete.cases(prt1),]
  #   if(input$Observations=='1'){
  #   }
  #   if (input$Observations=='2')
  #   {
  #     prtx=prt2[2,c("CL","V1","V2")]
  #     return(prtx)
  #   }
  # })
  
  # prelude 1. datasetInput ----
  
  datasetInput <- reactive({
    return(read_csv(default_dose_example_csv, col_types = 'cccd'))
  })
  
  # prelude 2. dose.data ----
  
  dose.data <- reactive({
    inFile <- input$file1 
    if (is.null(inFile)) return(NULL) 
    a=read.csv(inFile$datapath, header=T, stringsAsFactors = T) 
    b=a[complete.cases(a), ]  
    b$paste=paste(b$Date,b$Time) 
    b
    #Time calculation code is copyrighted
  })
  
  sim.data <- reactive({
    
    # prelude 3. sim.data ----
    input_file_text <- ifelse(is.null(input$file1), 
                              yes = default_dose_example_csv, 
                              no = input$file1$datapath)
    
    rawdata <- read_csv(input_file_text, col_types = 'cccd') %>% 
      as.data.frame() %>% 
      print()
    
    # input: Observation
    obs1conc <- input$obsc
    obs1time <- input$obsTime
    obs1date <- input$obsDate
    
    # Typical Values
    
    TVCL <- calculate_clearance(input$post_op_date) %>% 
      round(digits = 2)
    
    dosing_data <- rawdata %>% 
      mutate(date_time = sprintf('%sT%s', Date, Dosing_Time)) %>% 
      mutate(date_time_lubridate = ymd_hm(date_time)) %>% 
      mutate(Concentration = NA) %>% 
      select(date_time_lubridate, Dose, Concentration) %>% 
      as_tibble() %>% 
      print()
    
    observation_data <- tibble(
      date_time_lubridate = ymd_hms(sprintf('%sT%s', 
                                            obs1date, 
                                            substr(obs1time, 12, 20))),
      Dose = NA,
      Concentration = obs1conc
    )
    
    Dose_Concentration <- bind_rows(dosing_data, observation_data) %>% 
      mutate(first_dose = first(date_time_lubridate)) %>% 
      mutate(actual_time = difftime(date_time_lubridate, first_dose, unit = 'hour') %>% 
               as.numeric() %>% 
               round(2)) %>% 
      print()
    Concentration_only <- Dose_Concentration %>% 
      filter(!is.na(Concentration)) %>% 
      print()
    Dose_only <- Dose_Concentration %>% 
      filter(!is.na(Dose)) %>% 
      print()
    DOSEdata<- Dose_only %>% 
      mutate(var = 1, method = 'add') %>% 
      select(var, time = actual_time, value = Dose, method) %>% 
      as.data.frame() %>% 
      print()
    
    
    
    # model ----
    
    model <- function(Time, A, eta){
      Ka <- TVKA
      Cl <- TVCL * exp(eta[1])
      Vd <- TVV1 * exp(eta[2])
      
      Ke <-  Cl/Vd  # Elimination
      
      dA <- vector(length = 2)
      dA[1] <- -Ka * A[1]               # Depot compartment - intestine 
      dA[2] <-  Ka * A[1] - Ke * A[2]   # Central compartment - plasma
      return(list(dA))
    }
    
    mod.cmp <-  compiler::cmpfun(model)
    
    initial_amount <- c(A1 = 0, A2 = 0) 
    
    Observeddate <- Concentration_only$date_time_lubridate
    pointtime <- Concentration_only$actual_time
    TIME <- seq(from = 0, to = pointtime, by = 0.1)
    TIMElast <- max(TIME)
    
    if (input$Observations=='1') {
      y <- obs1conc / 1000
      #' @example mapb2(c(0.1, 0.1))
      mapb2 <- function(eta){ # eta is a list of 2. eta <- c(0.04321,0.04321)
        etamat=matrix(unlist(eta))
        out <- deSolve::lsoda(y = initial_amount, 
                              times = TIME, 
                              func = model, 
                              parms = eta, 
                              events=list(data=DOSEdata)) %>% 
          tibble::as_tibble() %>% 
          mutate(DV = A2/TVV1)
        
        eta <- c(eta[1],eta[2])
        eta_m <- unlist(matrix(eta,nrow = 2))
        sig2 <- EPS2SDsq
        sig2j <- subset(as.data.frame(out[,4]),out[,1]==pointtime)^2*sig2
        sqwres <- log(sig2j) + (1/sig2j) * 
          (y[1]-subset(as.data.frame(out[,4]),out[,1]==pointtime))^2
        nOn <- diag(t(eta_m) %*% omega.inv %*% eta_m)
        return(sum(sqwres)+ nOn)
      } 
    }
    
    mapb2.cmp <- cmpfun(mapb2)
    ini <- c(0.04,0.04) # eta initial value CL, V
    
    shiny::withProgress(
      message = 'Minimization in progress', 
      min = 0, max = 100, value = 99, {
        FIT <- stats::optim(par = ini, # CL, V => fitting
                            fn = mapb2.cmp, # A function to be minimized (or maximized)
                            method="L-BFGS-B", 
                            control = list(trace=TRUE,REPORT=TRUE))
        print(FIT$par)
      })
    
    outs <- lsoda(y = initial_amount, 
                  times = TIME, 
                  func = mod.cmp,  
                  parms = FIT$par,
                  events = list(data = DOSEdata)) %>% 
      as.data.frame() %>% 
      mutate(DV = A2/TVV1) %>% # mg / L == ug/mL == ng / uL
      print()
    
    if (input$Observations=='1'){
      outs$pointtime=pointtime  
      outs$Observeddate=Observeddate
      outs$predictedConc=subset(outs[,4],outs[,1]==pointtime)
      outs$observedConc=obs1conc
    }
    
    outs$TIME <- TIME
    outs2=merge(x=outs,y=DOSEdata, by="time",all.x=TRUE)
    
    table1_raw <- tibble(time = pointtime,
                         obs_conc = obs1conc,
                         pred_conc = subset(outs[,4],outs[,1]==pointtime) * 1000)
    
    tacrolimus_pk_plot <- outs2 %>% 
      as_tibble() %>% 
      ggplot(aes(time, DV*1000)) + 
      geom_point(data = table1_raw, 
                 aes(x=pointtime, y=pred_conc, color = 'Predicted'), 
                 size=4, alpha=0.5) + 
      geom_point(data = table1_raw, 
                 aes(x=pointtime, y=obs_conc, color = 'Observed'), 
                 size=4, alpha=0.5) + 
      geom_line(alpha = 0.8) +
      # geom_hline(yintercept = c(50, 200), color = 'red') +
      labs(x = 'Time (hour)', y = 'Whole blood tacrolimus concentration (ng/mL)',
           title = 'Oral administration of tacrolimus - surgical use',
           subtitle = 'Therapeutic Drug Monitoring',
           color = 'tacrolimus (ng/mL)') +
      theme_bw()
    tacrolimus_pk_plot
    
    sim_data_output <- list(
      table1 = table1_raw %>% select(`time (h)` = 1,
                                     `observed conc. (ng/mL)` = 2,
                                     `predicted conc. (ng/mL)` = 3),
      table2 = tibble(`CL/F (L/h)` = TVCL*(exp(FIT$par[1])),
                      `V/F (L)` = TVV1*(exp(FIT$par[2]))),
      plot1 = tacrolimus_pk_plot,
      # for plot2
      FIT = FIT,
      DOSEdata = DOSEdata,
      initial_amount = initial_amount,
      mod.cmp = mod.cmp
    )
    
    return(sim_data_output)
  })
  
  # plot 1 ----
  output$plotCONC <- renderPlot({
    sim_data_output <- sim.data()
    return(sim_data_output$plot1)
  }, res = 100)
  
  # plot 2 ----
  output$plotCONC2 <- renderPlot({
    sim_data_output <- sim.data()
    
    DOSEdata <- sim_data_output$DOSEdata
    initial_amount <- sim_data_output$initial_amount
    mod.cmp <- sim_data_output$mod.cmp
    FIT <- sim_data_output$FIT
    
    last_dose_actual_time <- max(DOSEdata$time)
    future_tau  <- input$newtau
    future_dose <- input$newdose
    
    DOSEdata_future <- bind_rows(DOSEdata,  
                                 tibble(seq = 1:10,
                                        actual_time = last_dose_actual_time + seq * future_tau,
                                        Dose = future_dose) %>% 
                                   mutate(var = 1, method = 'add') %>% 
                                   select(var, time = actual_time, value = Dose, method)) %>% 
      print()
    
    TIME_future <- seq(from = 0, to = max(DOSEdata_future$time) , by = 0.1)
    outs_future <- lsoda(y = initial_amount, 
                         times = TIME_future, 
                         func = mod.cmp,  
                         parms = FIT$par,
                         events = list(data = DOSEdata_future)) %>% 
      as.data.frame() %>% 
      mutate(DV = A2/TVV1) %>% # mg / L == ug/mL == ng / uL
      as_tibble() %>% 
      print()
    
    tacrolimus_dose_adjustment_plot <- outs_future %>% 
      as_tibble() %>% 
      ggplot(aes(time, DV*1000)) + 
      geom_line(alpha = 0.5) +
      # geom_hline(yintercept = input$ul, color = 'red') +
      # geom_hline(yintercept = input$ll, color = 'blue') +
      geom_vline(xintercept = last_dose_actual_time, color = 'red', alpha = 0.3) +
      labs(x = 'Time (hour)', y = 'Whole blood tacrolimus concentration (ng/mL)',
           title = 'Oral administration of tacrolimus - surgical use',
           subtitle = 'Therapeutic Drug Monitoring',
           color = 'tacrolimus (ng/mL)') +
      #scale_x_continuous(limits = c(max(DOSEdata$time), max(DOSEdata_future$time)))+
      theme_bw()
    tacrolimus_dose_adjustment_plot
    return(tacrolimus_dose_adjustment_plot)
  }, res = 100)
  # end ----  
})
