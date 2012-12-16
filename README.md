Reggy
=====

Flexible Erlang Process Registries

Allows starting multiple independant process registries.

Reggy is an application which allows one to start and use multiple process 
dictionaries for Erlang. The dictionaries provides a number of useful features beyond what 
the built-in dictionary has:

* Use multiple process registries.
* Use any term as a process alias.
* A process can have different aliases.

<pre>
reggy:start(my_registry),

...

reggy:register_name({my_registry, {worker, 1}, WorkerPid}),

...

Pid = reggy:whereis_name({my_registry, {worker, 1}}),

</pre>