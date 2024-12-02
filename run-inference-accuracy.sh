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
if [ $? -ne 0 ]; then
  echo "Build failed"
  exit 1
fi

echo "###############################"
echo "# POSITION INFERENCE ACCURACY #"
echo "###############################"

# These tests fix the particle counts of all tasks while varying the number of
# particles used in the positioning task. The goal is to show that more
# particles lead to a better accuracy but that it takes longer. Also, it shows
# the dependencies among tasks.

# We specify the number of particles to use in the positioning task (all other
# tasks performing inference run 1000 particles). When running many particles,
# we need to use a slowdown factor to ensure we have time to run the inference.
# This factor may need to be adjusted based on the computer on which we run the
# simulation.
PC=(100 1000 10000 100000)
SLOWDOWN=(1 1 1 10)

# 1. Set the task core mapping, allocating the tasks that perform inference
#    (pos and braking) to separate cores.
printf "core\npos 1\nbraking 2\nspeed 3\ndistance_SL 3\ndistance_SR 3\ndistance_FC 3\n" | python3 scripts/set-property.py ${BUILD_PATH}

# 2. Update the configuration of the braking task to use 1000 particles.
printf "particles\nbraking 1000\n" | python3 scripts/set-property.py ${BUILD_PATH}

# 3. Update the budgets of the pos and braking tasks to ensure both have
# sufficient time but neither (should) overrun their budgets.
printf "budget\npos 500000000\nbraking 250000000\n" | python3 scripts/set-property.py ${BUILD_PATH}

# 4. Run the simulation, varying the number of particles in each step.
for i in $(seq 0 3)
do
  # Set the number of particles to use in the positioning task for this
  # measurement.
  printf "particles\npos ${PC[i]}\n" | python3 scripts/set-property.py ${BUILD_PATH}

  mkdir -p measurements/particles-${i}

  # Repeat the simulation multiple times per particle configuration.
  for j in $(seq 1 100)
  do
    echo "Iteration ${j} (${PC[i]} particles)"

    OUT_DIR=measurements/particles-${i}/${j}
    mkdir -p ${OUT_DIR}

    # Run the simulation on the test data using the given configuration
    time sudo python3 scripts/runner.py -p ${BUILD_PATH} -m ${MAP_ID}.txt -r ${TEST_PATH} --record --slowdown ${SLOWDOWN[i]}

    python3 scripts/print-expected.py ${BUILD_PATH}/posDebug-actuator ${TRUE_POS_PATH} >> measurements/particles-${i}/accuracy.txt

    # Copy all files in the build directory to the output folder. We want to
    # keep most files for sanity checking, but the executables are not
    # important so we remove them (they also take up a lot of space).
    cp ${BUILD_PATH}/* ${OUT_DIR}
    find ${OUT_DIR} -type f -executable | xargs rm
  done
done

echo "#####################"
echo "# RESULT PROCESSING #"
echo "#####################"

# Two plots side-by-side: on the left-hand side, a box plot showing how the
# error decreases as we increase the number of particles in the pos task. On
# the right-hand side plot, we show the execution times.
python3 scripts/plot-box.py "${TEST_PATH}/true.txt"
