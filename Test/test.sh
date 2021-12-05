#! /usr/bin/env bash

# script's static configuration
################################################################################

# blarney's existing examples
ALL_BLARNEY_EXAMPLES=(
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
  Heat
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
  SizedStack
  Counter
  Vectors
)
BLARNEY_EXAMPLES="${BLARNEY_EXAMPLES[@]:-${ALL_BLARNEY_EXAMPLES[@]}}"

# per test set example exclusion lists
CIRCUIT_GEN_EXCLUDE=()
VERILATOR_EXCLUDE=(Heat)
HASKELL_SIM_EXCLUDE=(
  Bit0
  Spec
  Interface
  RAMQuad
)

# colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# control variables
################################################################################

doRunDefault=yup
doGenVerilog=
doRunCircuitGen=
doRunHaskellSim=
doRunVerilator=
doPluginNamer=
doPassNameProp=
doPassSimplifier=
verbose=0

# helper functions
################################################################################

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

# sub helper function
sub()
{
  awk -v a=$1 -v b=$2 'BEGIN {print a - b}'
}

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
      echo "--run-circuit-generation"
      echo "    runs blarney verilog circuit generator"
      echo "--run-haskell-simulation"
      echo "    runs the in haskell simulation backend"
      echo "--run-verilator"
      echo "    runs the verilog backend and a verilator simulation"
      echo "--plugin-namer"
      echo "    runs blc with the namer plugin enabled"
      echo "--pass-name-propagation"
      echo "    runs the circuit generator with the name propagation netlist pass enabled"
      echo "--pass-simplifier"
      echo "    runs the circuit generator with the netlist optimisation passes enabled"
      echo "--run-all"
      echo "    same as --run-circuit-generation, --run-haskell-simulation and --run-verilator"
      echo "--preserve-names"
      echo "    same as --plugin-namer and --pass-name-propagation"
      exit
      ;;
    --run-circuit-generation)
      doRunDefault=
      doRunCircuitGen=yup
      doGenVerilog=yup
      ;;
    --run-haskell-simulation)
      doRunDefault=
      doRunHaskellSim=yup
      ;;
    --run-verilator)
      doRunDefault=
      doRunVerilator=yup
      doGenVerilog=yup
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
    --run-all)
      doRunDefault=
      doRunCircuitGen=yup
      doRunHaskellSim=yup
      doRunVerilator=yup
      doGenVerilog=yup
      ;;
    --preserve-names)
      doPluginNamer=yup
      doPassNameProp=yup
      ;;
    -v|--verbose)
      verbose=$(add verbose  1)
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
if [ $doRunDefault ]; then
  doRunVerilator=yup
  doGenVerilog=yup
fi

# Build the blarney library and run regression tests
################################################################################

# prepare flags
BLC_FLAGS=()
if [ $doPluginNamer ]; then BLC_FLAGS+=("--enable-namer-plugin"); fi
GEN_FLAGS=()
if [ $doPassNameProp ]; then GEN_FLAGS+=("--enable-name-prop"); fi
if [ $doPassSimplifier ]; then GEN_FLAGS+=("--enable-simplifier"); fi

# tmp folder and reporting variables
now=$(date +%d.%m.%y-%H:%M)
tmpDir=$(mktemp -d -t blarney-test-$now-XXXX)

nbTests=0
failedTests=()

# prepare time accumulators
totalHaskellBuildTime=0
totalVerilogGenTime=0
totalVerilogBuildTime=0
totalVerilogSimRunTime=0
totalHaskellSimRunTime=0

# Build blarney
###############
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

# Run regression tests
######################
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
    # verilog circuit generation
    ############################
    if [ $doGenVerilog ]; then
      timeCmd ./$testName $GEN_FLAGS --verilog
      verilogGenTime=$(tail -n 1 $tmpTime)
      totalVerilogGenTime=$(add $totalVerilogGenTime $verilogGenTime)
    fi
    # test circuit generation
    #########################
    if [ $doRunCircuitGen ] && [[ ! " ${CIRCUIT_GEN_EXCLUDE[@]} " =~ " ${testName} " ]]; then
      printf "%-14s %16s" $testName "verilog-gen"
      printf "%10s" ""
      printf "%5s" "("
      if [ "$verbose" -gt "0" ]; then
        printf "ghc: %s, " $(showTime $haskellBuildTime)
      fi
      printf "gen: %s)\n" $(showTime $verilogGenTime)
    fi
    # test verilator
    ################
    if [ $doRunVerilator ] && [[ ! " ${VERILATOR_EXCLUDE[@]} " =~ " ${testName} " ]]; then
      printf "%-14s %16s" $testName "verilator-sim"
      timeCmd make -s -C $testName-Verilog &> $testName-test-verilator.log
      verilogBuildTime=$(tail -n 1 $tmpTime)
      totalVerilogBuildTime=$(add $totalVerilogBuildTime $verilogBuildTime)
      # Using 'sed \$d' to print all but the last line (works on Linux and OSX)
      # ('head -n -1' isn't available on OSX)
      timeCmd $testName-Verilog/$testName | sed \$d &> $testName-test-verilator.out
      verilogSimRunTime=$(tail -n 1 $tmpTime)
      totalVerilogSimRunTime=$(add $totalVerilogSimRunTime $verilogSimRunTime)
      # compare for result
      cmp -s $testName.out $testName-test-verilator.out
      if [ $? == 0 ]; then
        printf "${GREEN}%10s${NC}" "Passed"
      else
        printf "${RED}%10s${NC}" "Failed"
        failedTests+=("$testName-verilator ($exmplDir/$testName-test-verilator.{log, out})")
      fi
      # Counts available:
      #   $haskellBuildTime
      #   $verilogGenTime
      #   $verilogBuildTime
      #   $verilogSimRunTime
      # Display the most useful counts
      printf "%5s" "("
      if [ "$verbose" -gt "0" ]; then
        printf "ghc: %s, " $(showTime $haskellBuildTime)
      fi
      printf "gen: %s, " $(showTime $verilogGenTime)
      if [ "$verbose" -gt "0" ]; then
        printf "build: %s, " $(showTime $verilogBuildTime)
      fi
      printf "sim: %s)\n" $(showTime $verilogSimRunTime)
      nbTests=$(add $nbTests 1)
    fi
    # test simulation
    #################
    if [ $doRunHaskellSim ] && [[ ! " ${HASKELL_SIM_EXCLUDE[@]} " =~ " ${testName} " ]]; then
      printf "%-14s %16s" $testName "haskell-sim"
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
      # Counts available:
      #   $haskellBuildTime
      #   $haskellSimRunTime
      # Display the most useful counts
      printf "%5s" "("
      if [ "$verbose" -gt "0" ]; then
        printf "ghc: %s, " $(showTime $haskellBuildTime)
      fi
      printf "sim: %s)\n" $(showTime $haskellSimRunTime)
      nbTests=$(add $nbTests 1)
    fi
  done
  popd > /dev/null
done

# reporting
if [ $doRunCircuitGen ]; then
  printf '%.0s-' {1..80}
  printf '\n'
  printf "Verilog circuit generation cumulated times:\n"
  printf "ghc: %s" $(showTime $totalHaskellBuildTime)
  printf ", gen: %s\n" $(showTime $totalVerilogGenTime)
fi
if [ $doRunVerilator ]; then
  printf '%.0s-' {1..80}
  printf '\n'
  printf "Verilog backend cumulated times:\n"
  printf "ghc: %s" $(showTime $totalHaskellBuildTime)
  printf ", gen: %s" $(showTime $totalVerilogGenTime)
  printf ", verilation: %s" $(showTime $totalVerilogBuildTime)
  printf ", sim: %s\n" $(showTime $totalVerilogSimRunTime)
fi
if [ $doRunHaskellSim ]; then
  printf '%.0s-' {1..80}
  printf '\n'
  printf "Haskell Simulation backend cumulated times:\n"
  printf "ghc: %s" $(showTime $totalHaskellBuildTime)
  printf ", sim: %s\n" $(showTime $totalHaskellSimRunTime)
fi
nbFailedTests=${#failedTests[*]}
nbPassedTests=$(sub $nbTests $nbFailedTests)
printf '%.0s-' {1..80}
printf '\n'
echo -e "passed ${GREEN}$nbPassedTests${NC} tests (ran $nbTests)"
if [ $nbFailedTests -ne 0 ]; then
  echo -e "Failed ${RED}$nbFailedTests${NC} tests:"
  for i in ${!failedTests[*]}; do echo -e "  - ${failedTests[$i]}"; done
  exit -1
fi
