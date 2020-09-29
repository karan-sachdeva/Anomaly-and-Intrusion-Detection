Critical Infrastructures such as a power grid, thermal plants, water metering etc produce large
amounts of time series data. This project aimed to explore the data of electric consumption in
some foreign electrical power grid and figure out point and contextual anomalies. Point
anomalies were found out by general statistical methods such as standard deviation, comparing
means, maximum and minimum values. Feature selection was based on correlation coefficients
of the training data set.
Contextual anomalies were determined by building hidden markov models. The models were
built upon training data to replicate normal behavior and said models were used on test data
where their log likelihoods were compared to evaluate anomalous behaviour.
Based on our models, and the log likelihoods we determined that the test data sets is either quite
anomalous or our models are not an accurate representation of the normal behaviour.
