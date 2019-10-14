# Backtesting
Testing trading strategies

I am writing this post after being inspired to put my programming skills to some use in trading.

So here is my first attempt at backtesting a day trading strategy. I have tested an Opening range breakout strategy. It is a day trading strategy in which buy or sell calls are taken based on whether the price breaks out of the opening period high or low. The exact specifications are given below.

1) The stocks are filtered based on whether previous day's RSI was in a range between 30 to 70 on a 15 min timeframe.
2) The filtered stocks' opening 30 min range is noted
3) The 5 period EMA and 13 period EMA are calculated on the 5 min chart
4) When the 5 period EMA crosses the opening range high, the buy call is generated, the exact opposite happens in a Sell call.
5) The buy position is exited when the 5 min closing price goes below 13 period moving average both for taking profits and as a stop loss. The exact opposite happens when exiting a sell position.

Dataset tested on:
NIFTY 50 stocks from 19/12/2014 till 1/10/2015 (10 months approx.)

Constraints:
The number of trades per day per stock was limited to 3

Results:
Starting capital : 2,00,000
Max loss in a trade : -3763.3
Max profit in a trade : 9157.2
Average profit : 8.243 

Trading profit for 10 months : 1945.3
Number of trades taken (in 10 months): 539
Brokerage costs : 10780

Net Profit = 1945.3 - 10780 = -8834


