#
#

#' Experimental Design
#'
#' The function getExperiment() allows to run experiments over parameter spaces. The function calls the run() function for each parameter combination and writes the simulation results in a dataframe.
#'
#' @param authority.v Vector of logical values. Determines the mode of coordination. If TRUE then authority-based coordination is used.
#' @param ticks Numeric. The number of time steps running the simulation.
#' @param composability.v Vector of numeric values. Sets the degree of contribution interdependence between 0 and 1.
#' @param m.v Vector of numeric values. Determines the number of contribution created in each time step.
#' @param release.v Vector of numeric values. Sets the delay of visibility measured in time steps.
#' @param size Width (and breadth) of the design structure matrix to be created in the simulation. Default: 100.
#' @param repetitions  Numeric. Number of repetition for each parameter setting.
#' @param seed Numeric. Value for the random seed of the first experiment.
#' @param plot Logical. If TRUE then a plot is created displaying key measures of the design structure matrix during the simulation run ; default: FALSE
#' @return Dataframe of simulation runs.
#' @examples
#' getExperiment(authority.v=c(TRUE), ticks=1000, composability.v=c(0.070), m.v=c(100),f.v=c(1.00), turbulence.v=c(0), release.v=c(1), size=100, repetitions=1, seed=28071984, plot=FALSE)
#' getExperiment(authority.v=c(FALSE), ticks=1000, composability.v=c(0.070), m.v=c(100),f.v=c(1.00), turbulence.v=c(0), release.v=c(1), size=100, repetitions=1, seed=28071984, plot=FALSE)
#' getExperiment(authority.v=c(TRUE), ticks=1000, composability.v=c(seq(0.000,0.050,0.005)), m.v=c(100), f.v=c(1.00), turbulence.v=c(0), release.v=c(1), size=100, repetitions=1, seed=28071984, plot=FALSE)
#' @export
getExperiment <- function(authority.v,ticks,composability.v,m.v,f.v,turbulence.v,release.v,size=100,repetitions,seed,plot=FALSE) {
  print(paste("To do:",length(authority.v)*length(composability.v)*length(m.v)*length(f.v)*length(turbulence.v)*length(release.v)," runs with ",repetitions," repetitions. (",print(Sys.time()),")"))
  data <- NULL
  for(f in 1:length(authority.v)) {
    for(g in 1:length(composability.v)) {
      for(h in 1:length(m.v)) {
        for(i in 1:length(f.v)) {
          for(j in 1:length(turbulence.v)) {
            for(k in 1:length(release.v)) {
              for(l in size) {
                for(n in 1:repetitions) {
                  set.seed(seed+n)
                  data <- rbind(data,run(authority=authority.v[f],ticks=ticks,composability=composability.v[g],m=m.v[h],f=f.v[i],turbulence=turbulence.v[j],release=release.v[k],size=size,plot=plot)[[1]])
                }
              }
            }
          }
        }
      }
    }
  }
  print(Sys.time()); beep(sound=6)
  return(as.data.frame(data))
}

#' Visibility Matrix
#'
#' Creates a visibility matrix from a design structure matrix.
#'
#' Source: Sharman, D., Yassine, A., 2004. Characterizing complex product architectures. Systems Engineering Journal 7(1).
#'
#' @param techno.struct Design structure matrix.
#' @param N Width (or breadth) of the design structure matrix.
#' @return Visibility matrix as a matrix object.
#' @export
getVisibilityMatrix <- function(techno.struct,N) {
  ## Remove NAs:
  functions.na <- c()
  for(i in 1:nrow(techno.struct)) {
    if( any(is.na(techno.struct[i,])) ) {
      functions.na <- append(functions.na,i)
    }
  }
  if(length(functions.na)>0) { techno.struct <- techno.struct[-functions.na,-functions.na] }
  ## Calculate visibility matrix:
  o <- techno.struct %^% 0
  for(i in 1:N) {
    o <- o + techno.struct %^% i
  }
  o[o>0] <- 1
  return(o)
}

#' Propagation Costs
#'
#' Calculates the propagation costs of a design structure matrix.
#'
#' Sources: MacCormack, A., Rusnak, J., Baldwin, C.Y., 2006. Exploring the Structure of Complex Software Designs: An Empirical Study of Open Source and Proprietary Code. Management Science 52(7).
#'
#' Sharman, D., Yassine, A., 2004. Characterizing complex product architectures. Systems Engineering Journal 7(1).
#'
#' @param techno.struct Design structure matrix.
#' @return numeric value of propagation costs
#' @export
getPropagationCost <- function(techno.struct) {
  #techno.struct <- ifelse(techno.struct<1,0,1)
  return(sum(techno.struct)/(nrow(techno.struct)*ncol(techno.struct)))
}

#' Direct Coupling Costs
#'
#' Calculates the direct coupling costs of a design structure matrix.
#'
#' @param techno.struct Design structure matrix.
#'
#' @return numeric value of of direct coupling costs
#' @export
getDirectCouplingCost <- function(techno.struct) {
  ## Remove NAs:
  functions.na <- c()
  for(i in 1:nrow(techno.struct)) {
    if( any(is.na(techno.struct[i,])) ) {
      functions.na <- append(functions.na,i)
    }
  }
  if(length(functions.na)>0) { techno.struct <- techno.struct[-functions.na,-functions.na] }
  return(sum(techno.struct)/(nrow(techno.struct)*ncol(techno.struct)))
}

## Obsolete, not supported any more:
getVisibilityMatrix_pathLength <- function(techno.struct,N) {
  ## Remove NAs:
  functions.na <- c()
  for(i in 1:nrow(techno.struct)) {
    if( any(is.na(techno.struct[i,])) ) {
      functions.na <- append(functions.na,i)
    }
  }
  rownames(techno.struct) <- c(1:nrow(techno.struct))
  colnames(techno.struct) <- c(1:ncol(techno.struct))
  if(length(functions.na)>0) { techno.struct <- techno.struct[-functions.na,-functions.na] }
  ## Calculate visibility matrix:
  o <- techno.struct %^% N
  o[o>0] <- 1
  return(o)
}

## Obsolete, not supported any more:
getCyclicalGroups <- function(techno.struct,size) {
  ## Create visibility matrix:
  vis.m <- getVisibilityMatrix(techno.struct=techno.struct,N=size)
  ## Baldwin2014a Find Cyclic Groups: Step (1):
  vis.m <- vis.m[order(colSums(vis.m),decreasing=T),order(colSums(vis.m),decreasing=T)] # VFI
  vis.m <- vis.m[order(rowSums(vis.m),decreasing=F),order(rowSums(vis.m),decreasing=F)] # VFO
  ## Baldwin2014a Find Cyclic Groups: Step (2):
  cyclicGroups <- list()
  cyclicGroups.number <- 1
  for(i in 2:nrow(vis.m)) {

    if( sum(vis.m[i-1,])==sum(vis.m[i,])&sum(vis.m[,i-1])==sum(vis.m[,i])&sum(vis.m[i,])>1&sum(vis.m[,i])>1 ) {
      if( (sum(vis.m[i-2,])!=sum(vis.m[i,])|sum(vis.m[,i-2])!=sum(vis.m[,i]))&i-2>0 ) {
        cyclicGroups.number <- cyclicGroups.number+1
      }
      if(length(cyclicGroups)<cyclicGroups.number) {
        cyclicGroups[[cyclicGroups.number]] <- c(i-1,i)
      } else {
        cyclicGroups[[cyclicGroups.number]] <- append(cyclicGroups[[cyclicGroups.number]],c(i-1,i))
      }
    }
  }
  cyclicGroups <- cyclicGroups[!sapply(cyclicGroups,is.null)]
  if(length(cyclicGroups)>0) {
    for(j in 1:length(cyclicGroups)) {
      cyclicGroups[[j]] <- unique(cyclicGroups[[j]])
    }
  }
  ## Baldwin2014a Find Cyclic Groups: Step (3):
  if(length(cyclicGroups)>0) {
    cyclicGroups.toSplit <- c()
    for(i in 1:length(cyclicGroups)) {
      ##print( vis.m[min(cyclicGroups[[i]]):max(cyclicGroups[[i]]),min(cyclicGroups[[i]]):max(cyclicGroups[[i]])] )
      if(0 %in% vis.m[min(cyclicGroups[[i]]):max(cyclicGroups[[i]]),min(cyclicGroups[[i]]):max(cyclicGroups[[i]])]) {
        cyclicGroups.toSplit <- append(cyclicGroups.toSplit,i)
        ## Find Cyclic Groups: Step (4) Baldwin2014a, i.e. split:
      }
    }
    if(length(cyclicGroups.toSplit)>0) { cyclicGroups <- cyclicGroups[-cyclicGroups.toSplit] }
  }
  return(cyclicGroups)
}

#' Identifying Required Contributions
#'
#' @param sol.focal.pos Numeric. The position of the focal contribution for which requirements are searched for.
#' @param tec.struct The current design structure matrix.
#' @param sol.struct Dataframe containing the requirement vectors the contributions.
#' @param sol.functs Vector containing the functions of all contributions.
#' @param req.v Vector marking the required contributions.
#' @export
getTopdown.v <- function(sol.focal.pos,tec.struct,sol.struct,sol.functs,req.v=vector(mode="numeric",length=length(sol.functs))) {
  sol.focal.v <- sol.struct[,sol.focal.pos]
  for(i in 1:length(sol.focal.v)) {
    if(sol.focal.v[i]==1&sol.functs[sol.focal.pos]==i) {  ## focal is needed AND focal is current, THEN add current to req.
      req.v[sol.focal.pos] <- 1
    }
    if(sol.focal.v[i]==1&is.na(tec.struct[i,1])&sol.functs[sol.focal.pos]!=i&!i%in%sol.functs) { ## focal is needed AND focal is not in tec AND focal is not self-reference AND focal is not among sol THEN abort.
      req.v[sol.focal.pos] <- -999
      break
    }
    if(sol.focal.v[i]==1&is.na(tec.struct[i,1])&sol.functs[sol.focal.pos]!=i&i%in%sol.functs&req.v[sol.focal.pos]==0) {  ## focal is needed, focal is not in tec, focal is not current, focal is among sol, focal is not yet in req.v, THEN get current.
      req.v <- req.v + getTopdown.v(sol.focal.pos=which(i==sol.functs)[1],tec.struct,sol.struct,sol.functs,req.v=req.v)
    }
  }
  return(req.v)
}

#' Simulation Run
#'
#' Single simulation run of the simulation model.
#'
#' @param authority logical. Determines the mode of coordination. If TRUE then authority-based coordination is used.
#' @param ticks Numeric. The number of time steps running the simulation.
#' @param composability Numeric. Sets the degree of contribution interdependence between 0 and 1.
#' @param m Numeric. Determines the number of contribution created in each time step.
#' @param release Numeric. Sets the delay of visibility measured in time steps. Default: 1 (i.e. no release delay).
#' @param size Width (and breadth) of the design structure matrix to be created in the simulation. Default: 100.
#' @param plot Logical. If TRUE then a plot is created displaying key measures of the design structure matrix during the simulation run ; default: FALSE
#'
#' @return List containing measures of the simulation run.
#' @export
run <- function(authority,ticks,composability,m=1,f=1.00,turbulence=0,release=1,size=100,plot=T) {
  ### Initialize opportunities and technology:
  opport.effica <- vector()
  opport.functs <- vector()
  opport.struct <- matrix(nrow=size,ncol=0)
  opport.birtht <- vector()

  techno.effica <- vector(mode="numeric",length=size)
  techno.struct <- matrix(data=NA,nrow=size,ncol=size)
  techno.effica.obs <- techno.effica
  techno.struct.obs <- techno.struct

  ticks.whenturned80perc <- NA
  efficacy.cum <- 0
  complete.cum <- 0
  directCouplCost.at50perc <- NA
  propagationCost.at50perc <- NA
  numberOfTasksPulled <- vector(mode="numeric",length=ticks)

  if(plot) { x11()
             governance <- ifelse(authority,"td","bu")
             plot(0,0,xlab="time steps",ylab="Efficacy/Completeness",main=paste0(governance,",m:",m,",compo:",composability,"t:",turbulence,"r:",release,",f:",f,"\n size:",size),xlim=c(0,ticks*1),ylim=c(0,1),cex.main=0.9)
             abline(h=0.80,col="black",lty=2);abline(h=0.5,col="black",lty=2)
             abline(v=ticks/2,col="black",lty=2);abline(v=ticks/4,col="black",lty=2);abline(v=ticks*3/4,col="black",lty=2)
             abline(h=.2)
            }

  i <- 0
  while(i<ticks) {  ## sum(!is.na(techno.struct))/(size^2)<0.80
    ### Forget random old opportunities:
    if(length(opport.functs)>0) {
      toBeForgotten <- vector(mode="numeric",length=length(opport.functs))
      for(l in 1:length(toBeForgotten)) {
        if(runif(1,min=0,max=1)<=f) { toBeForgotten[l] <- 1 }
      } ; rm(l)

      opport.effica <- opport.effica[toBeForgotten!=1]
      opport.functs <- opport.functs[toBeForgotten!=1]
      opport.struct <- as.matrix(as.matrix(opport.struct)[,toBeForgotten!=1])
      opport.birtht <- opport.birtht[toBeForgotten!=1]
    }
    ### Deterioration of efficacies:
    techno.effica <- pmax(techno.effica-turbulence,0)
    toBeObsolte <- techno.effica==0
    techno.struct[toBeObsolte,] <- NA
    ### run through matrix and iteratively remove all unsupported functions.

    ### Add m ideas:
    opport.effica.new <- round(runif(m,min=1,max=100),0)
    opport.functs.new <- round(runif(m,min=1,max=size),0)
    opport.struct.new <- NULL
    for(j in 1:m) {
      opport.struct.new <- cbind(opport.struct.new,rbinom(size,1,composability)) ## model 054!
      ## opport.struct.new <- cbind(opport.struct.new,c(rbinom(100*g,1,composability*(1/g)-composability*(1-g)),rbinom(100*(1-g),1,composability*g))) ## robustness check A, model 055
      opport.struct.new[opport.functs.new[j],j] <- 1 ## Adding the self-reference.
    } ; rm(j)
    opport.birtht.new <- rep(i,times=m)

    opport.effica <- append(opport.effica,opport.effica.new)
    opport.functs <- append(opport.functs,opport.functs.new)
    opport.struct <- cbind(opport.struct,opport.struct.new)
    opport.birtht <- append(opport.birtht,opport.birtht.new)

    rm(opport.effica.new,opport.functs.new,opport.struct.new,opport.birtht.new)

    ### Remove all opportunities that are already implemented:
    tobeRemoved <- vector(mode="numeric",length=length(opport.functs))
    for(k in 1:length(opport.functs)) {
      if(techno.effica[opport.functs[k]]>0) {
        tobeRemoved[k] <- 1
      }
    } ; rm(k)

    opport.effica <- opport.effica[tobeRemoved!=1]
    opport.functs <- opport.functs[tobeRemoved!=1]
    opport.struct <- as.matrix(as.matrix(opport.struct)[,tobeRemoved!=1])
    opport.birtht <- opport.birtht[tobeRemoved!=1]

    ### PROCESSING:
    tempor.effica <- opport.effica
    tempor.functs <- opport.functs
    tempor.struct <- opport.struct

    i <- i + 1

    ### Delay:
    if(i>=release&i%%release==0) {
      techno.struct.obs <- techno.struct
      techno.effica.obs <- techno.effica
    }

    ### TOP-DOWN:
    if(authority) {
      while(length(tempor.functs)>0) {
        bestOpport.pos <- which(tempor.effica==max(tempor.effica))[1]
        toBeImplemented.v <- getTopdown.v(sol.focal.pos=bestOpport.pos,tec.struct=techno.struct.obs,sol.struct=tempor.struct,sol.functs=tempor.functs)

        if(any(toBeImplemented.v<0)) {
          ## Remove not implementable solutions:
          tempor.effica <- tempor.effica[toBeImplemented.v>=0]    ## tempor.effica <- tempor.effica[-c(bestOpport.pos)]
          tempor.functs <- tempor.functs[toBeImplemented.v>=0]     ##  tempor.functs <- tempor.functs[-c(bestOpport.pos)]
          tempor.struct <- as.matrix(tempor.struct[,toBeImplemented.v>=0]) ##  tempor.struct <- as.matrix(tempor.struct[,-c(bestOpport.pos)])
        }
        if(all(toBeImplemented.v>=0)&sum(toBeImplemented.v)>0) {
          ## Implement:
          numberOfTasksPulled[i] <- sum(toBeImplemented.v>0)
          for(l in 1:length(toBeImplemented.v)) {
            if(toBeImplemented.v[l]>=1) {
              techno.struct[tempor.functs[l],] <- tempor.struct[,l]
              techno.effica[tempor.functs[l]] <- tempor.effica[l]
            }
          } ; rm(l)
          tempor.effica <- tempor.effica[toBeImplemented.v>=0]
          tempor.functs <- tempor.functs[toBeImplemented.v>=0]
          tempor.struct <- as.matrix(tempor.struct[,toBeImplemented.v>=0])
          break
        }
      }
    }
    ### BOTTOM-UP:
    if(!authority) {
      techno.effica.freeze <- techno.effica.obs ### each opportunity is checked concurrently for implementation.
      while(length(tempor.functs)>0) {
        bestOpport.pos <- sample(1:length(tempor.effica),1) ## bestOpport.pos <- which(tempor.effica==max(tempor.effica))[1] ## random opportunity

        if(all(tempor.struct[,bestOpport.pos][-c(tempor.functs[bestOpport.pos])]<=techno.effica.freeze[-c(tempor.functs[bestOpport.pos])]) & techno.effica.freeze[tempor.functs[bestOpport.pos]]==0 & techno.effica[tempor.functs[bestOpport.pos]]==0 ) { ## (cuts out the current (best) opportunity of the function vector of that current (best) opportunity and the self reference in the architecture and checks in that way WHETHER ALL PRECONDITIONS ARE FULLFILLED ) &
            techno.struct[tempor.functs[bestOpport.pos],] <- tempor.struct[,bestOpport.pos]
            techno.effica[tempor.functs[bestOpport.pos]] <- tempor.effica[bestOpport.pos]
            numberOfTasksPulled[i] <- numberOfTasksPulled[i]+1
        }
        ## Remove best solution (whether implemented or not):
        tempor.effica <- tempor.effica[-c(bestOpport.pos)]
        tempor.functs <- tempor.functs[-c(bestOpport.pos)]
        tempor.struct <- as.matrix(tempor.struct[,-c(bestOpport.pos)])
      }
    }
    ### MEASURES:
    efficacy.cum <- efficacy.cum+sum(techno.effica,na.rm=TRUE)/(size^2)
    complete.cum <- complete.cum+sum(!is.na(techno.struct))/(size^2)
    propagationCost <- NA
    directCouplingCost <- NA
    Core.size <- NA

    if(sum(!is.na(techno.struct))/(size^2)>=0.80&is.na(ticks.whenturned80perc)) { ticks.whenturned80perc <- i }
    if(sum(!is.na(techno.struct))/(size^2)>=0.50&is.na(directCouplCost.at50perc)) {
      directCouplCost.at50perc <- getDirectCouplingCost(techno.struct=techno.struct) / (sum(!is.na(techno.struct))/(size^2))
      propagationCost.at50perc <- getPropagationCost(getVisibilityMatrix(techno.struct=techno.struct,N=size)) / (sum(!is.na(techno.struct))/(size^2))
    }


    if(plot) {
      if(sum(!is.na(techno.struct))>0.02*size^2) {
        directCouplingCost <- getDirectCouplingCost(techno.struct=techno.struct)
        propagationCost <- getPropagationCost(getVisibilityMatrix(techno.struct=techno.struct,N=size))
      }

      #points(x=i,y=sum(techno.effica,na.rm=TRUE)/(size*100),col="blue")
      points(x=i,y=sum(!is.na(techno.struct))/size^2,col="green")
      ##points(x=i,y=sum(techno.effica,na.rm=TRUE)/(i*100),col="purple") # efficacy by implementation

      # if(Core.size>0) { points(x=i,y=Core.size/length(techno.effica),col="red") } # size of largest cyclic group
      if(!is.na(directCouplingCost)) { points(x=i,y=directCouplingCost,col="yellow") }
      if(!is.na(propagationCost)) { points(x=i,y=propagationCost,col="orange",pch=0) }

      # if(i==799|i==800) {
      #   functions.na <- c()
      #   for(x in 1:nrow(techno.struct)) {
      #     if( any(is.na(techno.struct[x,])) ) {
      #       functions.na <- append(functions.na,x)
      #     }
      #   } ;rm(x)
      #   if(length(functions.na)>0) { techno.struct2 <- techno.struct[-functions.na,-functions.na] }
      #   write.matrix(techno.struct2,paste("tmpSlim_",i,".csv",sep=""),sep=",")
      # }
      #
      #   print(techno.struct2)
      #   print(directCouplingCost)
      #   print(propagationCost)
      #   print(techno.effica)
      #   print(paste(i," -----------------------------"))
      #   write.matrix(techno.struct2,paste("tmpSlim_",i,".csv",sep=""),sep=",")
      #
      #   rm(techno.struct2)
    }
}

  stringLengths <- rep(0,20) # size*0.2
  if(sum(!is.na(techno.struct))>0.02*size^2) {
    for(y in 1:length(stringLengths)) {
      stringLengths[y] <- sum(getVisibilityMatrix_pathLength(techno.struct,y))-sum(getVisibilityMatrix_pathLength(techno.struct,y-1))
    }
    ##print(stringLengths)
    #barplot(stringLengths,names.arg=c(1:length(stringLengths)),main=paste("auth:",authority,"compo:",composability,"m:",m,"f:",f,"size:",size,"t:",turbulence,"release:",release,sep=""),ylim=c(0,3000),ylab="Frequency",xlab="coupling lengths")

    directCouplingCost <- getDirectCouplingCost(techno.struct=techno.struct)
    propagationCost <- getPropagationCost(getVisibilityMatrix(techno.struct=techno.struct,N=size))
  }

  print(paste( "Run executed- complete:",sum(!is.na(techno.struct))/(size^2)," Direct coup:",round(directCouplingCost,2)," Indirct coup:",round(propagationCost,2)," Impl:",i," ticksTurned80perc:",ticks.whenturned80perc," compl.cum",round(complete.cum,2),"effica.cum",round(efficacy.cum,2),"auth:",authority,"compo:",composability,"m:",m,"f:",f,"size:",size,"t:",turbulence,"release:",release,"i:",i))
  result <- c(sum(techno.effica,na.rm=TRUE)/(size^2),sum(!is.na(techno.struct))/(size^2),directCouplingCost,propagationCost,ticks.whenturned80perc,efficacy.cum,complete.cum,directCouplCost.at50perc,propagationCost.at50perc,authority,composability,m,f,size,ticks,turbulence,release,i)
  result <- c(result,stringLengths,max(numberOfTasksPulled))
  names(result) <- c("effica","completeness","directCouplingCost","propagationCost","ticksTurned80perc","effica.cum","complete.cum","directCouplCost.at50perc","propagationCost.at50perc","authority","compo","m","f","size","ticks","turbulence","release","i","ic1st","ic2nd","ic3rd","ic4th","ic5th","ic6th","ic7th","ic8th","ic9th","ic10th","ic11th","ic12th","ic13th","ic14th","ic15th","ic16th","ic17th","ic18th","ic19th","ic20th")
  return(list(result,techno.struct))
}


