#!/bin/bash
nohup Rscript 3_training_strategy_under_M08_US10_ST1.r &> 3_training_strategy_under_M08_US10_ST100.log &
sleep 15m
nohup Rscript 4.HT_lightgbm_under_M08_US10_ST1.r &> 4.HT_lightgbm_under_M08_US10_ST100.log &
