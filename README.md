coord - Erlang/OTP distributed services startup coordination
============================================================

coord is an Erlang/OTP application to help coordinate distributed services
startup.


Distributed applications nodes can be started in a more flexible and elastic
way given the first node to be started can be flagged as such and manage
initial cluster setup.

coord uses [nodefinder](https://github.com/okeuday/nodefinder) for automatic
node discovery.

For an example application using coord to start a mnesia cluster you can
look at [remember](https://github.com/evax/remember).


Usage
-----

To use coord you have to include it in your dependencies:

```erlang
{deps, [
    ...
    {coord, ".*",
        {git, "git://github.com/evax/coord.git", "master"}}
]}.
```


Then define services in the coord section of your config file:

```erlang
[
    {coord, [
        {services, [
            {foo, 2000, coord_foo_service, []},
            {bar, 4000, coord_bar_service, []}
        ]}
    }
].
```


Services tuples are of the form:

```erlang
{Name :: atom(), DiscoveryTimeout :: integer(), ServiceModule :: atom() , Options :: list()}
```


In your service module, you'll have to implement the `coord_service` behaviour:

```erlang
init(First :: boolean(), Opts :: list()) -> State :: term().
nodeup(Node :: node(), State :: term() -> NewState :: term().
nodedown(Node :: node(), State :: term() -> NewState :: term().
```

