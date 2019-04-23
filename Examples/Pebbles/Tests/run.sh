#!/bin/bash

export PATH=$PATH:$(realpath ../bin)

VERILOG=../Pebbles-Verilog

if [ ! -f "$VERILOG/SimPebbles" ]; then
  echo Please build the simulator first
  exit -1
fi

TEST_DIR=$(realpath .)
make --quiet
for FILE in *.S; do
  TEST=$(basename $FILE .S)
  echo -ne "$TEST\t"
  cp $TEST.code.hex $VERILOG/prog.hex
  cp $TEST.data.0.hex $VERILOG/data_0.hex
  cp $TEST.data.1.hex $VERILOG/data_1.hex
  cp $TEST.data.2.hex $VERILOG/data_2.hex
  cp $TEST.data.3.hex $VERILOG/data_3.hex
  pushd . > /dev/null
  cd $VERILOG
  RESULT=$(./SimPebbles | head -n 1 | cut -d ' ' -f 2)
  popd > /dev/null
  if [ "$RESULT" == "0x00000001" ]; then
    echo "PASSED"
  else
    NUM=$(($RESULT/2))
    echo "FAILED $NUM"
    exit -1
  fi
done
