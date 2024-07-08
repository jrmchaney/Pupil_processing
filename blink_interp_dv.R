#for data export from eyelink dataviewer

blink_interp_dv <- function(data.good, spoint, fs, pre.pad, post.pad){
  
  blink.dat <- data.frame()
  #1b. get sub list
  sublist <- unique(data.good$subj)
  
  for(sub.now in sublist){
    
    #trim to current sub
    idx <- subset(data.good, subj == sub.now)
    
    #get subs list of trials
    tlist <- unique(idx$runTrial)
    
    sub.dat <- data.frame()
    #identify blinks in each trial
    for(tnow in tlist){
      
      idx2 <- idx %>% 
        subset(runTrial == tnow) %>% 
        mutate(pupil = ifelse(pupil == '.',
                                        yes = NA,
                                        no = pupil))
      
      #samples with blinks
      #blnk1 <- which(is.na(idx2$pupil))
      blnk1 <- which(idx2$blink==1)
      pres <- !length(blnk1)
      if(pres==TRUE){ #skip if this trial has no blinks (true means no blinks)
        
        idx2$Pupil = as.numeric(idx2$pupil)
        sub.dat <- rbind(sub.dat,idx2)
        
      }else{
      
        #blink start and end locations
        bStart <- blnk1[c(TRUE,diff(blnk1)!=1)]
        bEnd <- blnk1[c(diff(blnk1)!=1,TRUE)]
        
        #new blink start and ends (140ms on either side,  so  )
        pre.extend <- pre.pad/spoint #num of sample points to extend before
        post.extend <- post.pad/spoint #num samps to extend after blink
        bStartNew <- bStart - pre.extend
        bEndNew <- bEnd + post.extend
        
        #skip this trial if blink is at start of end of trial
        if(any(bStartNew <= 1)){
          next
        }
        if(any(bEndNew >= spTrial)){
          next
        }
        
        nblinks <- length(bStartNew)
        
        #make blink area NA with new start and end points
        temp.data <- data.frame()
        blnk.vec <- c()
        for(n in 1:nblinks){
          
          bvec <- c(bStartNew[n]:bEndNew[n])
          
          #remove this trial  if the blink is > 500ms
          # blen <- (length(bvec)/fs)*1000
          # if(blen > 500){
          #   next
          # }
          
          blnk.vec <- c(blnk.vec,bvec)
          
        } #nblinks
          
          #extend length of blink
          new.ix <- idx2 %>%
            replace_with_na(replace = list(sp= blnk.vec)) %>% 
            mutate(pupil = ifelse(is.na(sp) == TRUE,
                                            yes = NA,
                                            no = pupil))
          
          #linearly interpolate
          dat.now <- new.ix %>% 
            mutate(Pupil = na.approx(pupil))
          
          sub.dat <- rbind(sub.dat, dat.now)
      }#ifelse
      
    } #trial loop
    
    #add to larger data
    blink.dat <- rbind(blink.dat, sub.dat)
    
  } #sub loop
  
  return(blink.dat)
  
} #function