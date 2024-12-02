# Run a quick experiment to illustrate how execution time fairness is
# problematic when we have a task that runs inference very quickly, and other
# slower tasks depend on it.
#
# We run this using pre-recorded data on the Raspberry Pi.

BUILD_PATH=old-pos
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

echo "######################"
echo "# OLD MODEL FAIRNESS #"
echo "######################"

# These tests use the same importance for all three tasks performing inference
# (pos, braking, and speed), using the old version of the ProbTime positioning
# system where the speed task performs inference.
#
# The goal of this is to illustrate an issue that may arise when a quick task
# is allocated equal execution time to that of other, slower, tasks that depend
# on its result. In this case, the speed task will be allocated time to run
# many particles, which will cause the others to slow down due to them sampling
# from its resulting distribution.

printf "core\npos 0\nbraking 1\nspeed 2\ndistance_SL 3\ndistance_SR 3\ndistance_FC 3\n" | python3 scripts/set-property.py ${BUILD_PATH}

# Assign the same importance to all three tasks performing inference.
printf "importance\npos 1\nbraking 1\nspeed 1\ndistance_SL 0\ndistance_SR 0\ndistance_FC 0\n" | python3 scripts/set-property.py ${BUILD_PATH}

mkdir -p measurements/issue/

CONFIGURATIONS=("particle" "execution-time")
CONFIGURATION_FLAGS=("--particle-fairness" "--execution-time-fairness")
RUNNER="sudo python3 scripts/runner.py -p ${BUILD_PATH} -m ${MAP_ID}.txt -r ${TRAIN_PATH}"
REPETITIONS=3
for i in $(seq 0 1)
do
  FAIRNESS=${CONFIGURATIONS[i]}
  CONFIG_FLAGS=${CONFIGURATION_FLAGS[i]}
  echo "Evaluating ${FAIRNESS} fairness"

  OUTDIR=measurements/issue/${FAIRNESS}
  mkdir -p ${OUTDIR}

  rtppl-configure --repetitions ${REPETITIONS} --safety-margin 0.85 --path ${BUILD_PATH} --runner "${RUNNER}" ${CONFIG_FLAGS} > ${OUTDIR}/log.txt

  cp ${BUILD_PATH}/system.json ${OUTDIR}/system.json
done
