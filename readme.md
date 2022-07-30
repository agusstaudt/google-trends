# Daily google trends functions 
Google trends indicators have become very popular in recent years. Such information has been used as input to perform different prediction exercises, from estimating a country's economic activity in real time to predicting the daily price of cryptocurrencies. However, obtaining historical daily information for each word or topic searched is not a trivial task. Google queries allow daily windows only for word requests shorter than 9 months, which prevents obtaining historical indicators in daily unit in a direct way. In this framework, the following project develops the `daily_gt` function that converts the different sets of daily indicators into a single comparable daily historical indicator. The function is performed both in R and in Python. 