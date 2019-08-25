../../Scripts/blc PipelineTest.hs || { echo 'Compilation failed' ; exit 1; }
./PipelineTest || { echo 'FSM Construction failed' ; exit 1; }
cd Pipeline-Test
make ./top
./top
