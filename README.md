Map Reduce Server (MRS)
=======================

This project is a demonstration of how to implement a simple map-reduce system in Erlang/OTP. It's not intended to be used as a production system (indeed, it doesn't even have error handling). The purpose is purely educational.

Erlang lends itself well to such a project for a number of reasons:
* parallelism across physical machines in a cluster comes for free
* the language is functional with higher order functions that can be passed around between processes and machines
* actor model concurrency works very well for this sort of task

Thanks to these and other benefits, the whole working system is in just two source files, currently just over 100 lines of code including comments, and whitespace. A handful of ready-to-run implementations of simple map-reduce algorithms are also available in the "compute.erl" module. 

The source code files
---------------------

There are currently four files in the /src folder:
* bootstrap.erl - some simple util functions to make it easier to get a cluster running.
* compute.erl - contains sample MapReduce algorithms
* mrs.erl - represents a central query coordinator for the distributed system.
* worker.erl - represents a worker process that will store integers and process map commands from the mrs server.

Single Node operation
---------------------

Simply run "start_server.bat" from the command line. This will start an Erlang session and launch the mapreduce server with a few workers and some seed data. To inspect the data and workers, call mrs:print/0. Run the sample mapreduce algorithms by calling the methods in the compute module, for example compute:sum/0.

Multi-node operation
--------------------

TODO: Write this...