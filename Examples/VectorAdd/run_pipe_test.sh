blc VectorAdd.hs || { echo 'Compilation failed' ; exit 1; }
./VectorAdd || { echo 'FSM Construction failed' ; exit 1; }
cd VectorAdd-Test
make ./top
./top
