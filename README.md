# Bank Simulation
In this exercise, we are going to explore your simulation capabilities. There will be some light math involved but the bulk of what we are looking for is your ability to model problems in code.

You are allowed and encouraged to search the internet for prior art and use any library that you deem helpful but please don't plagiarize the work of others. Some search terms that are relevant for this task: *Monte Carlo, Beta distributions, Critical-event technique, Time-slice method*

### Background

Our bank is a rather poor bank and can only afford one teller to man the station at any given time. Customers enter, seemingly at random and form a queue in front of the teller. The bank manager has tasked us to model this problem so he can get some answers. 

Here is the model for the probability for how customers arrive into the bank. This model assumes that customers arrive one at a time. `F(t)` gives us the probability that a customer arrives at any given `t` and this probability tends to `1` over time.  We can set the value of `𝛼 = 100` for  this simulation.

![customer arrival](./docs/customer-arrival.png)

Given the seconds since the last customer arrived as `t` , this equation gives you the probability (between 0 and 1) that a customer arrives at this second. *Hint: See if you can rewrite this equation such that you only need to do one random number roll to get the answer*

As soon as a customer reaches the head of the queue, they are served by the teller. Since the problems of each customer can take a different amount of time to execute, we model this via a beta distribution like so:

![customer service](./docs/customer-service.png)

You can assume the value of ρ = 200  . To get the processing time for the customer, generate a random value between 0 and 1 and set x to it. The result of the equation will be the time in seconds that this customer takes to process.

There are 3  different types of customers: yellow, red and blue, each of their time taken is modeled by the same equation but with different values of alpha and beta. You can assume that all customers coming in will be of the same type for any given simulation. 


### Task
Your task is to write a program that can outputs to stdout the answers to the following questions:

- Given only yellow customers, what are the average and maximum customer waiting times?
- Given only red customers, what are the average and maximum queue lengths in-front of the teller?
- Which type of customer(yellow, red or blue) gives the closest value between the average and maximum customer waiting times?

### Submission

You can submit your solution by putting it on Github and sending us a link. Ping us if you have any questions on problem. We expect this modelling to take no more than 4-6 hours of your time.