Map Reduce Server (MRS)
=======================

The source code files
---------------------

There are currently four files in the /src folder:
* bootstrap.erl - some simple util functions to make it easier to get a cluster running.
* mrs.erl - represents a central query coordinator for the distributed system.
* slave.erl - some simple util functions to make it easier to get a cluster running.
* worker.erl - represents a worker process that will store integers and process map commands from the mrs server.

Single Node operation
---------------------

TODO: Write this...

Multi-node operation
--------------------

TODO: Write this...