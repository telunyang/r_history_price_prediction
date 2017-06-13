#引入 Crawler 相關函式庫
library(RSelenium)

#處理字串用函式庫
library(stringr)

#引入 RODBC 函式庫
library(RODBC)

#啟動瀏覽器
rsDr <- rsDriver()
remDr <- rsDr[["client"]]

#http://www.wine-searcher.com/market-127-2007

#建立連線子
conn <- odbcDriverConnect('driver={SQL Server}; server=DESKTOP-GFE9KED\\SQLEXPRESS; database=demo; trusted_connection=true')

#查詢資料並取得集合
res <- sqlQuery(
		conn, 
		"select top 5 [sn],[wineid_number],[vintage] 
		 from [demo].[dbo].[wine_data]")


#將參數帶入網址，並加以走訪
for(i in 1:nrow(res))
{
	#在 Wine Searcher 的酒編號
	wineid_number <- res[i, 2]
	
	#酒年份
	vintage <- res[i, 3]
	
	#整理網址字串
	url <- sprintf("http://www.wine-searcher.com/market-%d-%d", wineid_number, vintage)

	print(url)
	
	#設定網頁連結
	remDr$navigate(url)
	
	#取得網址內容
	html <- remDr$getPageSource()[[1]]
	
	#取得貨幣資料
	result_currency <- str_match_all(html, "curCode[\\s=]+'([a-zA-Z]+)")
	currency <- result_currency[[1]][1,2]
	print(currency)
	
	#去掉不必要的資料(避免正式資料誤判)
	changed_html <- str_replace_all(html, "var[\\s\\S]arrChartRgnGrp[\\s\\S]+\\[Date\\.UTC\\(([0-9]+),([0-9]+),([0-9]+)\\),([0-9]+)\\]];", "")
	
	#取得酒價格資訊
	result_content <- str_match_all(changed_html, "\\[Date\\.UTC\\(([0-9]+),([0-9]+),([0-9]+)\\),([0-9]+)\\]")
	
	# 用 ym 作為 x 標題的數量 (計量變數)
	count <- 0
	
	#取得近兩年內，每一個月的酒價格 (這裡是關鍵)
	for(j in 1:nrow(result_content[[1]]))
	{		
		year <- result_content[[1]][j,2]
		month <- result_content[[1]][j,3]
		price <- result_content[[1]][j,5]
		
		#insert 用 sql 字串
		insert_sql <- sprintf(
				"INSERT INTO wine_price (wineid_number, vintage, year, month, price, currency) VALUES (%d, %d, %d, %d, %d, '%s')",
				wineid_number,
				vintage,
				strtoi(year),
				(strtoi(month)+1),
				strtoi(price),
				currency)
		
		#寫入資料庫/表
		sqlQuery(conn, insert_sql)
		
		#累計計數
		count <- count + 1;
	}
	
	#產出圖表
	if( count >= 2)
	{
		#準備用來產出圖表所查詢的 sql 字串
		chart_sql <- sprintf(
				"select [sn],
						[wineid_number],
						[vintage],
						[year],
						[month],
						[price],
						[currency],
						concat([year], '-', [month]) as [ym]
				from [demo].[dbo].[wine_price]
				where [wineid_number] = %d and [vintage] = %d 
				order by [year] asc, [month] asc", wineid_number, vintage)
		
		#查詢資料集合
		data <- sqlQuery(conn, chart_sql)
		
		# 匯入圖形套件
		library(ggplot2)
		
		# 建立 x 軸的向量資料，方便進行斜率計算
		x_vector <- c(1:count)
		
		# 建立模型
		myModel <- lm(price ~ x_vector, data)
		
		# 截距
		intc <- coef(myModel)[1]
		
		# 斜率
		slp <- coef(myModel)[2]
		
		# 製作圖表 (記得安裝字型 )
		chart <- ggplot(data, aes(x = data$ym, y = data$price))
		chart <- chart + geom_point() 
		chart <- chart + geom_abline(intercept = intc, slope = slp, color = "red")
		chart <- chart + geom_smooth(method = "lm", se = FALSE)
		chart <- chart + ggtitle("Price Prediction")
		chart <- chart + labs(x = "Year-Month", y = "Price")
		chart <- chart + theme(text = element_text(family = ""), 
				axis.text.x = element_text(size = 5, angle = 90), 
				axis.text.y = element_text(size = 10), 
				axis.title = element_text(size = 14))
	
		
		# 圖表路徑
		file_path <- paste(wineid_number, vintage, sep = "-")
		file_path <- paste("C:\\Users\\darren\\workspace\\R-Crawler-Wine-Price-Prediction\\images\\", file_path, ".png", sep = "")
		
		# 儲存圖片
		ggsave(file_path)
	}
	
	#計量變數初始化
	count <- 0
	
	#讓程式等若干秒後再運作，以免被擋
	Sys.sleep(10)
}


#關閉資料庫
odbcClose(conn)

#關閉瀏覽器
remDr$close()
