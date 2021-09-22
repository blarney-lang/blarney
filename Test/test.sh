#! /usr/bin/env bash

# script's static configuration
################################################################################

BLARNEY_DEFAULT_EXAMPLES=(
  Background
  BasicRTL
  Bit0
  BitPat
  BitScan
  CPU
  Derive
  FIFO
  Factorial
  Interface
  Lookup
  MasterSlave
  MeanFilter
  NameBits
  NoC
  Option
  Queue
  RAM
  RAMBE
  RAMQuad
  Sorter
  SourceSink
  Stack
  Counter
  Vectors
)
BLARNEY_EXAMPLES="${BLARNEY_EXAMPLES[@]:-${BLARNEY_DEFAULT_EXAMPLES[@]}}"

# exclude those from being tested, for whatever reason (mainly expected failure)
VERILOG_EXCLUDE=()
SIMULATION_EXCLUDE=(
  Bit0
  Spec
  Interface
  RAMQuad
)

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# control variables
################################################################################

doBackendDefault=yup
doBackendSimulation=
doBackendVerilog=
doPluginNamer=
doPassNameProp=
doPassSimplifier=
verbose=0

# preliminary checks and script arguments processing
################################################################################

if [ -z "$BLARNEY_ROOT" ]; then
  echo Please set the BLARNEY_ROOT environment variable
fi
BLARNEY_TESTING_ROOT="${BLARNEY_TESTING_ROOT:-$BLARNEY_ROOT}"

while :
do
  case $1 in
    ############################################################################
    -h|--help)
      echo "Runs the blarney examples as a regression test suite"
      echo "--backend-simulation"
      echo "    runs the in haskell simulation backend"
      echo "--backend-verilog"
      echo "    runs the verilog backend and a verilator simulation"
      echo "--plugin-namer"
      echo "    runs blc with the namer plugin enabled"
      echo "--pass-name-propagation"
      echo "    runs the circuit generator with the name propagation netlist pass enabled"
      echo "--pass-simplifier"
      echo "    runs the circuit generator with the netlist optimisation passes enabled"
      echo "--backend-all"
      echo "    same as --backend-simulation and --backend-verilog"
      echo "--preserve-names"
      echo "    same as --plugin-namer and --pass-name-propagation"
      exit
      ;;
    --backend-simulation)
      doBackendDefault=
      doBackendSimulation=yup
      ;;
    --backend-verilog)
      doBackendDefault=
      doBackendVerilog=yup
      ;;
    --plugin-namer)
      doPluginNamer=yup
      ;;
    --pass-name-propagation)
      doPassNameProp=yup
      ;;
    --pass-simplifier)
      doPassSimplifier=yup
      ;;
    --backend-all)
      doBackendDefault=
      doBackendSimulation=yup
      doBackendVerilog=yup
      ;;
    --preserve-names)
      doPluginNamer=yup
      doPassNameProp=yup
      ;;
    -v|--verbose)
      verbose=$((verbose + 1))
      ;;
    ############################################################################
    -?*)
      printf 'Ignoring unknown flag: %s\n' "$1" >&2
      ;;
    --)
      shift
      break
      ;;
    *)
      break
  esac
  shift
done
# assign a default backend if necessary
if [ $doBackendDefault ]; then doBackendVerilog=yup; fi

# Run regression tests
################################################################################

# prepare flags
BLC_FLAGS=()
if [ $doPluginNamer ]; then BLC_FLAGS+=("--enable-namer-plugin"); fi
GEN_FLAGS=()
if [ $doPassNameProp ]; then GEN_FLAGS+=("--enable-name-prop"); fi
if [ $doPassSimplifier ]; then GEN_FLAGS+=("--enable-simplifier"); fi

# reporting variables
nbTests=0
failedTests=()

# tmp folder
now=$(date +%d.%m.%y-%H:%M)
tmpDir=$(mktemp -d -t blarney-test-$now-XXXX)

# timing helpers:
# need to explicitly avoid shell built-in time command to use the -o/--output
# and -f/--format flags
theTimeCmd=$(which time)
# The $tmpTime file stores the result of the last call to timeCmd()
tmpTime=$(mktemp -t time-XXXX)
# times a command
timeCmd()
{
  $theTimeCmd -o $tmpTime -f %e $@
}
# displays an arbitrary duration in seconds
showTime()
{
  local t1=$(echo $1 | cut -d '.' -f1)
  local t2=$(echo $1 | cut -d '.' -f2)
  if [ "$t1" -lt "3600" ]; then date -u +%M:%S.$t2 -d @$t1
  elif [ "$t1" -lt "86400" ]; then date -u +%H:%M:%S.$t2 -d @$t1
  else echo "$t1.$t2 seconds, more than a day..."
  fi
}
# displays the content of $tmpTime
showLastTime()
{
  showTime $(tail -n 1 $tmpTime)
}

# add helper function
add()
{
  # echo "$1 + $2" | bc
  # Note: bc cannot display leading 0 for 0.xxx values... Use awk instead
  awk -v a=$1 -v b=$2 'BEGIN {print a + b}'
}

# start with building the blarney library itself
pushd $BLARNEY_ROOT > /dev/null
echo -n "Blarney build (blc): "
make clean > /dev/null
tmpLog=$tmpDir/blarney-initial-blc-build.log
timeCmd make blc-build &> $tmpLog
if [ $? != 0 ]; then
  echo -e "${RED}KO${NC} - ($(showLastTime))"
  echo "content of $tmpLog:"
  cat $tmpLog
  exit -1
else echo -e "${GREEN}OK${NC} - ($(showLastTime))"
fi
printf '%.0s=' {1..80}
printf '\n'
popd > /dev/null

# prepare time accumulators
totalHaskellBuildTime=0
totalVerilogGenTime=0
totalVerilogBuildTime=0
totalVerilogSimRunTime=0
totalHaskellSimRunTime=0
# go through each examples
for E in ${BLARNEY_EXAMPLES[@]}; do
  # work in a temporary directory
  ###############################
  exDir=$BLARNEY_TESTING_ROOT/Examples/$E
  exmplDir=$tmpDir/blarney-test-$E
  mkdir -p $exmplDir
  cp -r $exDir/* $exmplDir/.
  pushd $exmplDir > /dev/null
  # run each test in the blarney example
  ######################################
  outputs=$(ls *.out)
  for O in $outputs; do
    testName=$(basename $O .out)
    # build the blarney example
    ###########################
    timeCmd make -s BLC_FLAGS=$BLC_FLAGS $testName &> build.log
    if [ $? != 0 ]; then
      echo -e "${RED}Failed to build $testName${NC}"
      echo "content of $exmplDir/build.log:"
      cat $exmplDir/build.log
      exit -1
    fi
    haskellBuildTime=$(tail -n 1 $tmpTime)
    totalHaskellBuildTime=$(add $totalHaskellBuildTime $haskellBuildTime)
    # test verilog
    ##############
    if [ $doBackendVerilog ] && [[ ! " ${VERILOG_EXCLUDE[@]} " =~ " ${testName} " ]]; then
      printf "%-12s %12s" $testName "verilog"
      timeCmd ./$testName $GEN_FLAGS --verilog
      verilogGenTime=$(tail -n 1 $tmpTime)
      totalVerilogGenTime=$(add $totalVerilogGenTime $verilogGenTime)
      timeCmd make -s -C $testName-Verilog &> $testName-test-verilog.log
      verilogBuildTime=$(tail -n 1 $tmpTime)
      totalVerilogBuildTime=$(add $totalVerilogBuildTime $verilogBuildTime)
      # Using 'sed \$d' to print all but the last line (works on Linux and OSX)
      # ('head -n -1' isn't available on OSX)
      timeCmd $testName-Verilog/$testName | sed \$d &> $testName-test-verilog.out
      verilogSimRunTime=$(tail -n 1 $tmpTime)
      totalVerilogSimRunTime=$(add $totalVerilogSimRunTime $verilogSimRunTime)
      # compare for result
      cmp -s $testName.out $testName-test-verilog.out
      if [ $? == 0 ]; then
        printf "${GREEN}%10s${NC}" "Passed"
      else
        printf "${RED}%10s${NC}" "Failed"
        failedTests+=("$testName-verilog ($exmplDir/$testName-test-verilog.{log, out})")
      fi
      printf " (haskell build: %s" $(showTime $haskellBuildTime)
      printf ", verilog gen: %s" $(showTime $verilogGenTime)
      printf ", verilog build: %s" $(showTime $verilogBuildTime)
      printf ", verilog sim: %s)\n" $(showTime $verilogSimRunTime)
      nbTests=$((nbTests+1))
    fi
    # test simulation
    #################
    if [ $doBackendSimulation ] && [[ ! " ${SIMULATION_EXCLUDE[@]} " =~ " ${testName} " ]]; then
      printf "%-12s %12s" $testName "simulation"
      timeCmd ./$testName $GEN_FLAGS --simulate &> $testName-test-sim.out
      haskellSimRunTime=$(tail -n 1 $tmpTime)
      totalHaskellSimRunTime=$(add $totalHaskellSimRunTime $haskellSimRunTime)
      # compare for result
      cmp -s $testName.out $testName-test-sim.out
      if [ $? == 0 ]; then
        printf "${GREEN}%10s${NC}" "Passed"
      else
        printf "${RED}%10s${NC}" "Failed"
        failedTests+=("$testName-sim ($exmplDir/$testName-test-sim.out)")
      fi
      printf " (haskell build: %s" $(showTime $haskellBuildTime)
      printf ", haskell sim: %s)\n" $(showTime $haskellSimRunTime)
      nbTests=$((nbTests+1))
    fi
  done
  popd > /dev/null
done

# reporting
if [ $doBackendVerilog ]; then
  printf '%.0s-' {1..80}
  printf '\n'
  printf "Verilog backend cumulated times:\n"
  printf "haskell build: %s" $(showTime $totalHaskellBuildTime)
  printf ", verilog gen: %s" $(showTime $totalVerilogGenTime)
  printf ", verilog build: %s" $(showTime $totalVerilogBuildTime)
  printf ", verilog sim: %s\n" $(showTime $totalVerilogSimRunTime)
fi
if [ $doBackendSimulation ]; then
  printf '%.0s-' {1..80}
  printf '\n'
  printf "Haskell Simulation backend cumulated times:\n"
  printf "haskell build: %s" $(showTime $totalHaskellBuildTime)
  printf ", haskell sim: %s\n" $(showTime $totalHaskellSimRunTime)
fi
nbFailedTests=${#failedTests[*]}
nbPassedTests=$((nbTests-nbFailedTests))
printf '%.0s-' {1..80}
printf '\n'
echo -e "passed ${GREEN}$nbPassedTests${NC} tests (ran $nbTests)"
if [ $nbFailedTests -ne 0 ]; then
  echo -e "Failed ${RED}$nbFailedTests${NC} tests:"
  for i in ${!failedTests[*]}; do echo -e "  - ${failedTests[$i]}"; done
  exit -1
fi
