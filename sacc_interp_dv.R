sacc_interp_dv <- function(blink.dat, spoint, fs, sacc.pad, spTrial){
  
  fixed.dat <- data.frame()
  #1b. get sub list
  sublist <- unique(blink.dat$subj)
  
  for(sub.now in sublist){
    
    #trim to current sub
    idx <- subset(blink.dat, subj == sub.now)
    
    #get subs list of trials
    tlist <- unique(idx$runTrial)
    
    sub.dat <- data.frame()
    #identify saccades in each trial
    for(tnow in tlist){
      
      #if  saccade and  blink occurred at same time, ignore because we already fixed blinks
      idx2 <- idx %>% 
        subset(runTrial == tnow) %>% 
        mutate(sp = 1:spTrial) %>% 
        mutate(saccade = ifelse(saccade == 1 & blink == 1,
                                        yes = 0,
                                        no = saccade)) %>%
        mutate(Pupil = ifelse(saccade == 1,
                                        yes = NA,
                                        no = Pupil))
      
      #samples with SACCADES
      sacc1 <- which(idx2$saccade==1)
      pres <- !length(sacc1)
      if(pres == TRUE){ #skip if this trial has no saccades
        sub.dat <- rbind(sub.dat, idx2)
      }else{
        
        #sacc start and end locations
        sStart <- sacc1[c(TRUE,diff(sacc1)!=1)]
        sEnd <- sacc1[c(diff(sacc1)!=1,TRUE)]
        
        #new sacc start and ends (100ms on either side,  so  )
        extend <- sacc.pad/spoint #num of sample points to extend
        sStartNew <- sStart - extend
        sEndNew <- sEnd + extend
        
        #fix if this trial if saccade is at start of end of trial
        if(any(sStartNew <= 1)){
          # start <- which(sStartNew <= 1)
          # sStartNew[start] <- 2
          next
        }
        if(any(sEndNew >= spTrial)){
          #find which one
          # end <- which(sEndNew >= spTrial)
          # sEndNew[end] <- spTrial-1
          next
        }
        
        nsaccs <- length(sStartNew)
        
        #make saccade area NA with new start and end points
        temp.data <- data.frame()
        sacc.vec <- c()
        for(n in 1:nsaccs){
          
          svec <- c(sStartNew[n]:sEndNew[n])
          sacc.vec <- c(sacc.vec,svec)
          
        } #nsaccs
          
          #extend length of saccade
          new.ix <- idx2 %>%
            replace_with_na(replace = list(sp= sacc.vec)) %>% 
            mutate(Pupil = ifelse(is.na(sp) == TRUE,
                                            yes = NA,
                                            no = Pupil))
          
          #linearly interpolate
          dat.now <- new.ix %>% 
            mutate(Pupil = na.approx(Pupil)) %>% 
            mutate(sp = 1:spTrial)
          
          sub.dat <- rbind(sub.dat, dat.now)
      }
      
    } #trial loop
    
    #add to larger data
    fixed.dat <- rbind(fixed.dat, sub.dat)
    
  } #sub loop
  
  fixed.dat <- fixed.dat %>% 
    select(-(blink:pupil))
  
  return(fixed.dat)
  
} #function