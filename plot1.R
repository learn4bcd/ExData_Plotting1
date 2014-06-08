## This scripts is for plotting plot1.png, 
## which shows distribution of "Global Active Power (kilowatts)" by histogram.


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

## plotting ...
png("plot1.png",width = 480, height = 480, units = "px",bg=NA)

hist(as.numeric(data[,3]),main="Global Active Power",
     xlab="Global Active Power (kilowatts)",col="red")

dev.off()