# RTPPL Experiments

This repository contains the version of the experiment code used for the RTSS 2024 submission of the paper "Real-Time Probabilistic Programming".

## Installing

To be able to run these tests, you need to install Miking, Miking DPPL, and ProbTime. As these are research projects which may introduce breaking changes over time, we recommend using specific branches of them which have been tested to work with the experiment code:
* [Miking](https://github.com/larshum/miking/tree/rtss-2024)
* [Miking DPPL](https://github.com/larshum/miking-dppl/tree/rtss-2024)
* [ProbTime](https://github.com/larshum/probtime/tree/rtss-2024)

## Running

The scripts at the root of the repository are used to run the simulated experiments presented in the paper. We do not have a script for the live measurements on the RC car, as these measurements are ran manually.

The three runner scripts are as follows:
* `run-automatic-configuration.sh`: Evaluates the result of varying the importance ratio of tasks using execution-time fairness and particle fairness. The particle counts are found in the `system.json` file for each test, while the measured WCETs are stored in `pos.txt` and `braking.txt`, respectively.
* `run-fairness-issue.sh`: Evaluates the configuration on a version of the positioning and braking model where the `speed` task also performs inference. The resulting particle count of the three tasks are stored in the `system.json` file after configuration.
* `run-inference-accuracy.sh`: Evaluates the accuracy when varying the particle count of the `pos` task. The `braking` task is fixed to 1000 particles, while we using varying amounts of particles in the `pos` task. For each particle count in `pos`, we run 100 simulations on the test data and compare the final result with the true x- and y-position of the test data (which we measured beforehand). The resulting accuracy is stored in a file `accuracy.txt` in the directory of each test.
