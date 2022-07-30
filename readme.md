# Daily google trends functions 
[***Google trends indicators***](https://trends.google.es/trends/?geo=AR) have become very popular in recent years. Such information has been used as input to perform different prediction exercises, from estimating a [***country's economic activity in real time***](https://www.oecd-ilibrary.org/economics/tracking-activity-in-real-time-with-google-trends_6b9c7518-en) to predicting the [***daily price of cryptocurrencies***](https://www.nature.com/articles/srep03415). However, obtaining historical daily information for each word or topic searched is not a trivial task. Google queries allow daily windows only for word requests shorter than 9 months, which prevents obtaining historical indicators in daily unit in a direct way. In this framework, the following project develops the `daily_gt` function that converts the different sets of daily indicators into a single comparable daily historical indicator, following the next [***article***](https://medium.com/@bewerunge.franz/google-trends-how-to-acquire-daily-data-for-broad-time-frames-b6c6dfe200e6). The function is performed both in R and in Python. 
