#!/bin/bash
nohup Rscript z932_training_strategy_under_oneshot.r &> z932_training_strategy_under_oneshot.log &
sleep 15m
nohup Rscript z942_HT_lightgbm_under_oneshot.r &> z942_HT_lightgbm_under_oneshot.log &
