#!/bin/bash
nohup Rscript 3_training_strategy_under_M09_US25_ST100.r &> 3_training_strategy_under_M09_US25_ST100.log &
sleep 15m
nohup Rscript 4.HT_lightgbm_under_M09_US25_ST100.r &> 4.HT_lightgbm_under_M09_US25_ST100.log &
