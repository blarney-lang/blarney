#! /usr/bin/env bash

# Run regression tests

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

if [ -z "$BLARNEY_ROOT" ]; then
  echo Please set BLARNEY_ROOT environment variable
fi
BLARNEY_TESTING_ROOT="${BLARNEY_TESTING_ROOT:-$BLARNEY_ROOT}"

# Fresh start
make -s -C $BLARNEY_TESTING_ROOT clean

# Test plugins and netlist passes?
if [ "$1" == "full" ]; then
  echo "Testing plugins and netlist passes"
  BLC_FLAGS="--enable-namer-plugin"
  GEN_FLAGS="--enable-name-prop --enable-simplifier"
fi

NB_TESTS=0
FAILS=()
for E in ${BLARNEY_EXAMPLES[@]}; do
  # work in a temporary directory
  EX_DIR=$BLARNEY_TESTING_ROOT/Examples/$E
  TMP_DIR=$(mktemp -d -t blarney-test-$E-XXXX)
  cp -r $EX_DIR/* $TMP_DIR/.
  pushd $TMP_DIR > /dev/null
  # run each test in the blarney example
  OUTPUTS=$(ls *.out)
  for O in $OUTPUTS; do
    TEST=$(basename $O .out)
    echo "- $TEST: "
    # build the blarney example
    make -s BLC_FLAGS=$BLC_FLAGS $TEST &> /dev/null
    if [ $? != 0 ]; then
      echo "${RED}Failed to build $TEST${NC}"
      exit -1
    fi
    # test verilog
    echo -ne "\tverilog - "
    ./$TEST $GEN_FLAGS --test-verilog-gen
    make -s -C $TEST-Verilog &> /dev/null
    # Using 'sed \$d' to print all but the last line (works on Linux and OSX)
    # ('head -n -1' isn't available on OSX)
    $TEST-Verilog/$TEST | sed \$d > $TEST-test-verilog.out
    compare_outputs $TEST.out $TEST-test-verilog.out || FAILS+=("$TEST-verilog")
    NB_TESTS=$((NB_TESTS+1))
    # test simulation
    echo -ne "\tsimulation - "
    ./$TEST $GEN_FLAGS --test-simulation > $TEST-test-sim.out
    compare_outputs $TEST.out $TEST-test-sim.out || FAILS+=("$TEST-sim")
    NB_TESTS=$((NB_TESTS+1))
  done
  popd > /dev/null
done

NB_FAILS=${#FAILS[@]}
echo "ran $NB_TESTS tests"
if [ $NB_FAILS -ne 0 ]; then
  echo -e "${RED}Failed $NB_FAILS tests:${NC}"
  for t in ${FAILS[@]}; do echo -e "\t- $t"; done
  exit -1
fi
