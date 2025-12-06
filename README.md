Monte Carlo Simulation Applications
This repository contains several small projects demonstrating how Monte Carlo simulation can be used in quantitative finance, numerical analysis, and probability.​

Features
Asian option pricing
Simulates stock price paths under a geometric Brownian motion model and estimates the fair value of Asian options by averaging discounted payoffs over many paths.​

Stock price prediction
Uses historical returns to estimate drift and volatility, then runs Monte Carlo paths to obtain a distribution of possible future stock prices and summary statistics such as mean, variance, and confidence intervals.​

Distribution transformation
Implements standard techniques (such as inverse transform and related sampling methods) to generate random variables from non‑uniform distributions starting from uniform pseudo‑random numbers.​

Monte Carlo integration
Approximates definite integrals, including higher‑dimensional ones, by sampling random points and averaging the function values, highlighting the dimension‑independent convergence rate of Monte Carlo methods.​

Option valuation and other integrals
Shows how option prices and similar quantities can be written as expectations and estimated with Monte Carlo, turning complex integrals (for example, those involving lognormal distributions) into simulation problems.​

Technologies
Python (NumPy, pandas, Matplotlib/Seaborn) for simulation and visualization.​

Basic stochastic calculus and probability concepts (e.g., geometric Brownian motion, random sampling, variance reduction ideas).
