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
  OptionExample
  Queue
  RAM
  RAMBE
  Sorter
  SourceSinkStream
  Stack
  UpDownCounter
  Vectors
)
BLARNEY_EXAMPLES="${BLARNEY_EXAMPLES[@]:-${BLARNEY_DEFAULT_EXAMPLES[@]}}"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# control variables. Default: simulation backend only
################################################################################

doBackendSimulation=yup
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
      echo "-s/--backend-simulation"
      echo "    runs the in haskell simulation backend (currently always on)"
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
    -s|--backend-simulation)
      doBackendSimulation=yup
      ;;
    --backend-verilog)
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

# helper functions
################################################################################

compare_outputs () {
  cmp -s $1 $2
  if [ $? == 0 ]; then
    echo -e "${GREEN}Passed${NC}"
    return 0
  else
    echo -e "${RED}Failed${NC}"
    return 1
  fi
}

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

for E in ${BLARNEY_EXAMPLES[@]}; do
  # work in a temporary directory
  ###############################
  exDir=$BLARNEY_TESTING_ROOT/Examples/$E
  tmpDir=$(mktemp -d -t blarney-test-$E-XXXX)
  cp -r $exDir/* $tmpDir/.
  pushd $tmpDir > /dev/null
  # run each test in the blarney example
  ######################################
  outputs=$(ls *.out)
  for O in $outputs; do
    testName=$(basename $O .out)
    echo "- $testName"
    # build the blarney example
    ###########################
    make -s BLC_FLAGS=$BLC_FLAGS $testName &> /dev/null
    if [ $? != 0 ]; then
      echo "${RED}Failed to build $testName${NC}"
      exit -1
    fi
    # test verilog
    ##############
    if [ $doBackendVerilog ]; then
      echo -n "  verilog: "
      ./$testName $GEN_FLAGS --test-verilog-gen
      make -s -C $testName-Verilog &> /dev/null
      # Using 'sed \$d' to print all but the last line (works on Linux and OSX)
      # ('head -n -1' isn't available on OSX)
      $testName-Verilog/$testName | sed \$d > $testName-test-verilog.out
      compare_outputs $testName.out $testName-test-verilog.out || failedTests+=("$testName-verilog")
      nbTests=$((nbTests+1))
    fi
    # test simulation
    #################
    if [ $doBackendSimulation ]; then
      echo -n "  simulation: "
      ./$testName $GEN_FLAGS --test-simulation > $testName-test-sim.out
      compare_outputs $testName.out $testName-test-sim.out || failedTests+=("$testName-sim")
      nbTests=$((nbTests+1))
    fi
  done
  popd > /dev/null
done

# reporting
nbFailedTests=${#failedTests[@]}
echo "ran $nbTests tests"
if [ $nbFailedTests -ne 0 ]; then
  echo -e "${RED}Failed $nbFailedTests tests:${NC}"
  for t in ${failedTests[@]}; do echo -e "\t- $t"; done
  exit -1
fi
