## This scripts is for plotting plot3.png, 
## which shows the distribution of three different "Energy sub meterings"
## against "datetime" from 2007-02-01 00:00:00 to 2007-02-02 23:59:00


# This function is for reading data from raw file in a memory-efficient
# manner. It only loads lines matched to pattern assigned by the 'pattern'
# parameter, and returns a matrix containing 9 columns corresponding to:
# Date  Time    Global_active_power Global_reactive_power   Voltage
# Global_intensity    Sub_metering_1  Sub_metering_2  Sub_metering_3

readData <- function(infile = "household_power_consumption.txt",
                     pattern = "^([1-2])/2/2007",
                     splitstring = ";"){
    
    filehandle <- file(infile,"r")
    
    header <- readLines(filehandle,n=1)
    header <- strsplit(header,split=splitstring)[[1]]
    data <- t(data.frame(row.names = header))
    # Date  Time    Global_active_power Global_reactive_power   Voltage
    # Global_intensity    Sub_metering_1  Sub_metering_2  Sub_metering_3

    tag <- 0
    
    while(T){
        
        thisLine <- readLines(filehandle,n=1)
        
        if(length(thisLine) == 0) break
        
        if (length(grep(pattern,thisLine)) ){
            # for time-saving, do not use perl = T ...
            tag <- 1
            values <- strsplit(thisLine,split=splitstring)[[1]]
            
            ## for some 'wrong' records,ex.
            ## 22/2/2007;22:58:00;?;?;?;?;?;?;
            if (length(values) != 9) values <- c(values,rep("?",9))[1:9]
            
            data <- rbind(data, values)
        }else{
            if (tag == 1) break
        }
    }
    
    close(filehandle)
    
    rownames(data) <- c()
    
    return(data)
}

data <- readData()

## data processing ...
data <- as.data.frame(data,stringsAsFactors = F)
data[,7] <- as.numeric(data[,7])
data[,8] <- as.numeric(data[,8])
data[,9] <- as.numeric(data[,9])

data <- cbind(data, 
              datetime=strptime(paste(data[,1],data[,2],sep=" "),
                                "%d/%m/%Y %H:%M:%S")
              )

## plotting ...
png("plot3.png",width = 480, height = 480, units = "px",bg=NA)

with(data,{
     plot(datetime,Sub_metering_1,col="black",type="l",xlab="",
          ylab="Energy sub metering")
     lines(datetime,Sub_metering_2,col="red",type="l")
     lines(datetime,Sub_metering_3,col="blue",type="l")
     legend("topright", lty=1,col = c("black","red","blue"), 
            legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        }
     )

dev.off()