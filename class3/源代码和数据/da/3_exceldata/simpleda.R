#--------------------------------------------------#
# 第2讲: 简单数据处理和分析                        #
# Author：猴子
# 社群：大数据思维社群，微信公众号（猴子聊人物）   #
#--------------------------------------------------#

install.packages("openxlsx")
library("openxlsx")

install.packages("stringr")
library(stringr)

#------------------------------------------------
#读取excel数据
#------------------------------------------------
readFilePath <- "C:/朝阳医院2016年销售数据.xlsx"
excelData <- read.xlsx(readFilePath,"Sheet1")

#------------------------------------------------
#数据预处理
#------------------------------------------------

#step1:列名重命名
names(excelData) <- c("time","cardno",
                      "drugId","drugName",
                      "saleNumber","virtualmoney",
                      "actualmoney")

#ste2:删除缺失数据
excelData <- excelData[!is.na(excelData$time),]

#step3:处理日期
timeSplit <- str_split_fixed(excelData$time, " ",n=2)
excelData$time <-timeSplit[,1]
#字符串转换为日期格式
excelData$time <- as.Date(excelData$time, 
                          "%Y-%m-%d")

#step4:数据类型转换
#销售数量
excelData$saleNumber <- as.numeric(excelData$saleNumber)
#应收金额（打折前的金额）
excelData$virtualmoney <- as.numeric(excelData$virtualmoney)
#实收金额（打折后的金额）
excelData$actualmoney <- as.numeric(excelData$actualmoney)

#step5:数据排序
#按销售时间对数据进行升序排序
excelData <- excelData[order(excelData$time,
                             decreasing = FALSE),]



#-------------------------------------------------------
# 业务指标1：月均消费次数
#-------------------------------------------------------

kpi1 <- excelData[!duplicated(
                  excelData[,c("time","cardno")]
                  ),]

#总消费次数
consumeNumber <- nrow(kpi1)

#月份数

#最小的时间值
startTime <- kpi1$time[1]
#最大的时间值
endTime <- kpi1$time[nrow(kpi1)]

#天数
day <- as.numeric(endTime -startTime)
#月份数
#如果你想计算的更准确，可以将一个月有31天或者28天的情况也考虑进去。这里为了简单说明计算方式，没有考虑复杂的情况。
month <- day %/% 30

#月均消费次数
monthConsume <- consumeNumber / month

monthConsume <- format(round(monthConsume, 2), nsmall = 2)

#-------------------------------------------------------
# 业务指标2：月均消费金额
#-------------------------------------------------------
totalMoney <- sum(excelData$actualmoney,na.rm=TRUE)

monthMoney <- totalMoney / month

#-------------------------------------------------------
# 业务指标3：客单价pct(per customer transaction) 
#-------------------------------------------------------

pct <- totalMoney / consumeNumber

pct <- format(round(pct, 2), nsmall = 2)

#-------------------------------------------------------
# 业务指标4：消费曲线图
#-------------------------------------------------------
week <- tapply(excelData$actualmoney, 
               format(excelData$time, "%Y-%U"), 
               sum) 

week <- as.data.frame.table(week)

names(week) <- c("time","actualmoney")

week$time <- as.character(week$time)
week$timeNumber <- c(1:nrow(week))

plot(week$timeNumber, week$actualmoney,
     xlab="时间（年份-第几周）",
     ylab="消费金额",     
     xaxt = "n",
     main= "2016年朝阳医院消费曲线", 
     col="blue",
     type="b")

axis(1,at=week$timeNumber, labels=week$time, cex.axis = 1.5)


