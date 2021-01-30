-module(master_node).

%% API
-export([start/0]).

master_loop(Count, []) ->
  timer:sleep(2000),
  ActiveHosts = setup(),
  master_loop(Count, ActiveHosts);

master_loop(Count, Hosts) ->
  receive
    {From, Msg} ->
      Index = (Count rem length(Hosts)) + 1,
      Host = lists:nth(Index, Hosts),
      io:format("Received: ~p from ~p\n", [Msg, From]),
      io:format("rpc on ~p\n", [Host]),
      case rpc:call(Host, test_runner, test, [Msg]) of
        {badrpc, _} ->
          io:format("Detected host ~p down\n", [Host]),
          self() ! {From, Msg},
          ActiveHosts = lists:delete(Host, Hosts),
          io:format("Active hosts: ~p\n", [ActiveHosts]),
          master_loop(Count, ActiveHosts);
        Reply ->
          io:format("Got: ~p\n", [Reply]),
          From ! Reply,
          master_loop(Count+1, Hosts)
      end
  end.

read_config(File) ->
  {ok, ConfBin} = file:read_file(File),
  ConfString = binary:bin_to_list(ConfBin),
  Hosts = string:split(ConfString, "\n", all),
  hosts_to_atom(Hosts).

hosts_to_atom(Hosts) ->
  hosts_to_atom(Hosts, []).

hosts_to_atom([], Atoms) -> Atoms;
hosts_to_atom([H|T], Atoms) ->
  hosts_to_atom(T, Atoms ++ [list_to_atom(H)]).

connect_to_hosts(Hosts) ->
  connect_to_hosts(Hosts, []).

connect_to_hosts([], ActiveHosts) -> ActiveHosts;
connect_to_hosts([H|T], ActiveHosts) ->
  case net_adm:ping(H) of
    pong -> connect_to_hosts(T, ActiveHosts ++ [H]);
    pang -> connect_to_hosts(T, ActiveHosts)
  end.

setup() ->
  Hosts = read_config("turtle.conf"),
  io:format("Configured hosts: ~p\n", [Hosts]),
  ActiveHosts = connect_to_hosts(Hosts),
  io:format("Active hosts: ~p\n", [ActiveHosts]),
  ActiveHosts.

start() -> start(master).
start(Name) ->
  ActiveHosts = setup(),
  Pid = spawn(node(), fun() -> master_loop(0, ActiveHosts) end),
  register(Name, Pid).