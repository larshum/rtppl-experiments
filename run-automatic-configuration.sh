# Runs the automatic configuration experiments, where the ratio of importance
# between the positioning and the braking tasks is varied. The purpose of this
# evaluation is to show the difference between the execution time fairness and
# the particle fairness approaches.
#
# We run this on the Raspberry Pi with pre-recorded data. A surprising benefit
# of using the RPi for this benchmark is that configuration actually runs
# faster than on a desktop machine, because a slower CPU decreases the size of
# the search space we need to consider (while each configuration step runs in a
# fixed amount of time for given pre-recorded data).

BUILD_PATH=positioning
MAP_ID=maps/corridor
TRAIN_PATH=measurements/train_data
TEST_PATH=measurements/test_data
TRUE_POS_PATH=${TEST_PATH}/true.txt

# Sanity check: ensure data for testing and training exists
if [ ! -d ${TRAIN_PATH} ]; then
  echo "Training data measurements directory not found (${TRAIN_PATH})"
  exit 1
fi
if [ ! -d ${TEST_PATH} ]; then
  echo "Test data measurements directory not found (${TEST_PATH})"
  exit 1
fi

# Create the textual map representation used by the ProbTime program if it does
# not already exist.
if [ ! -f ${MAP_ID}.txt ]; then
  python3 scripts/map-conv.py ${MAP_ID}.png > ${MAP_ID}.txt
fi

# Rebuild the ProbTime program, removing any previous files in the build path
# if it had already been compiled.
if [ -f ${BUILD_PATH}/system.json ]; then
  python3 scripts/clean.py -p ${BUILD_PATH} -a
fi
python3 scripts/build.py ${BUILD_PATH}
if [ $? -ne 0]; then
  echo "Build failed"
  exit 1
fi

echo "###########################"
echo "# AUTOMATIC CONFIGURATION #"
echo "###########################"

# This experiment tests the outcome of the automatic configuration when varying
# the ratio of importance assigned to the pos and braking tasks. We start with
# both tasks having importance one, and double the importance of one task until
# we reach a point where we cannot find a schedulable configuration satisfying
# the given constraints. We perform this for both tasks and using both
# approaches to fairness.
#
# For each measurement, we collect the resulting number of particles per task
# (pos and braking) and the configuration time (in terms of the number of
# iterations).

# 1. Set the task to core mapping such that the tasks that perform inference
#    are on the same core, and the other tasks are on another core.
printf "core\npos 1\nbraking 2\nspeed 3\ndistance_SL 3\ndistance_SR 3\ndistance_FC 3\n" | python3 scripts/set-property.py ${BUILD_PATH}

CONFIGURATIONS=("particle" "execution-time")
CONFIGURATION_FLAGS=("--particle-fairness" "--execution-time-fairness")
TASKS=("pos" "braking")
RUNNER="sudo python3 scripts/runner.py -p ${BUILD_PATH} -m ${MAP_ID}.txt -r ${TRAIN_PATH}"
REPETITIONS=3
for i in $(seq 0 1)
do
  FAIRNESS=${CONFIGURATIONS[i]}
  CONFIG_FLAGS=${CONFIGURATION_FLAGS[i]}
  echo "Evaluating ${FAIRNESS} fairness"
  for j in $(seq 0 1)
  do
    TASK=${TASKS[j]}

    mkdir -p measurements/${FAIRNESS}

    echo "Increasing the importance of the ${TASK} task"

    # Overwrite the initial importance values in the system.json file such that
    # the pos and braking tasks are assigned importance one and the other tasks
    # have importance zero (as they perform no inference).
    printf "importance\npos 1\nbraking 1\nspeed 0\ndistance_SL 0\ndistance_SR 0\ndistance_FC 0\n" | python3 scripts/set-property.py ${BUILD_PATH}

    RES=0
    PPOS=1
    PBRAKING=1
    while :
    do
      # We double at the end if j = 0 and at the start if j = 1, so that we
      # only run the 1 to 1 ratio configuration once.
      if [ "$j" -eq "1" ]; then
        PBRAKING=$((2 * PBRAKING))
      fi

      OUTDIR=measurements/${FAIRNESS}/${PPOS}-${PBRAKING}
      mkdir -p ${OUTDIR}

      # Set the importance values of tasks in the configuration file.
      printf "importance\npos ${PPOS}\nbraking ${PBRAKING}\nspeed 0\ndistance_SL 0\ndistance_SR 0\ndistance_FC 0\n" | python3 scripts/set-property.py ${BUILD_PATH}

      rtppl-configure --repetitions ${REPETITIONS} --safety-margin 0.85 --path ${BUILD_PATH} --runner "${RUNNER}" ${CONFIG_FLAGS} > ${OUTDIR}/log.txt

      # If the configuration failed, e.g., because the ratio between the
      # importance of the tasks is too large, we stop the experiment here.
      if [ "$?" -ne "0" ]; then
        printf "\nMax ratio: ${PPOS} ${PBRAKING}\n"
        break
      else
        printf "."
      fi

      # Copy the resulting configuration file
      cp ${BUILD_PATH}/system.json ${OUTDIR}/system.json

      # Run the configured system three times to estimate the WCETs of the pos
      # and braking tasks
      for k in $(seq 1 ${REPETITIONS})
      do
        # Run the configured system using test data (distinct from the training
        # data we use to configure the system).
        sudo python3 scripts/runner.py -p ${BUILD_PATH} -m ${MAP_ID}.txt -r ${TEST_PATH} --record >> ${OUTDIR}/run-log.txt 2>&1

        # Extract the WCETs from the .collect files of the respective tasks and
        # store this in a file.
        cat ${BUILD_PATH}/pos.collect | sort -n | tail -n 1 >> ${OUTDIR}/pos.txt
        cat ${BUILD_PATH}/braking.collect | sort -n | tail -n 1 >> ${OUTDIR}/braking.txt
      done

      if [ "${j}" -eq "0" ]; then
        PPOS=$((2 * PPOS))
      fi
    done
  done
done

echo "#####################"
echo "# RESULT PROCESSING #"
echo "#####################"

python3 scripts/plot-autoconfig.py
