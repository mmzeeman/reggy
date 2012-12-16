Reggy
=====

Flexible Erlang Process Registries

Reggy is an application which allows one to start and use multiple process 
registries for Erlang. The registries provides a number of useful features beyond what 
the built-in registry has:

* Use multiple process registries.
* Use any term as a process alias.
* A process can have different aliases.

<pre>
reggy:start(my_registry),
...
reggy:register_name({my_registry, {worker, 1}, WorkerPid}),
...
Pid = reggy:whereis_name({my_registry, {worker, 1}}),
...
</pre>