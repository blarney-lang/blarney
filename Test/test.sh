#!/bin/bash

# Run regression tests

EXAMPLES=(
  BasicRTL
  NameBits
  BitPat
  BitScan
  CPU
  Derive
  Factorial
  FIFO
  Interface
  MeanFilter
  Queue
  SourceSinkStream
  RAM
  Sorter
  UpDownCounter
  MasterSlave
  NoC
  Background
  Stack
)

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

if [ -z "$BLARNEY_ROOT" ]; then
  echo Please set BLARNEY_ROOT environment variable
fi

for E in "${EXAMPLES[@]}"; do
  cd $BLARNEY_ROOT/Examples/$E
  # previous 'stack build' run already created the executables
  if [ "$BLARNEY_TEST_USING_STACK_BUILD" == 1 ]; then
    # nop - built with 'stack build' already
    echo "" >/dev/null
  else
    # uses Scripts/blc to compile
    make -s &> /dev/null
    if [ $? != 0 ]; then
      echo Failed to build $E
      exit -1
    fi
  fi
  for O in $(ls *.out); do
    TEST=$(basename $O .out)
    echo -ne "$TEST: "
    if [ "$BLARNEY_TEST_USING_STACK_BUILD" == 1 ]; then
      # run from local-install-root directory
      # ('stack build' created the bin/blarney-example-* binarys)
      SP=$(stack path | grep local-install-root | cut -d " " -f 2)
      if [ -f $SP/bin/blarney-example-$TEST ]; then
        $SP/bin/blarney-example-$TEST
      else
        echo "Please run 'stack build' to create the example/test binaries first."
        exit 1
      fi
    else	
      ./$TEST
    fi
    cd $TEST-Verilog
    make -s &> /dev/null
    # Using 'sed \$d' to print all but the last line (works on Linux and OSX)
    # ('head -n -1' isn't available on OSX)
    ./top | sed \$d > $TEST.got
    cd ..
    cmp -s $TEST.out $TEST-Verilog/$TEST.got
    if [ $? == 0 ]; then
      echo -e "${GREEN}Passed${NC}"
    else
      echo -e "${RED}Failed${NC}"
      exit -1
    fi
  done
done
