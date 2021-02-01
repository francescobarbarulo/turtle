-module(master_node).

%% API
-export([start/0]).

master_setup() ->
  process_flag(trap_exit, true),
  setup(),
  master_loop(0).

master_loop(Count) ->
  io:format("[*] Wainting for a message...\n"),
  receive
    {FromMailbox, Msg} ->
      Index = (Count rem length(nodes())) + 1,
      TargetNode = lists:nth(Index, nodes()),
      io:format("\n---------------------------------------------\n"),
      io:format("[>] Received new job from ~p\n", [FromMailbox]),
      io:format("[>] Spawning new process on node ~p\n", [TargetNode]),
      Master = self(),
      spawn_link(TargetNode, test_runner, start, [Master, {FromMailbox, Msg}]),
      master_loop(Count+1);

    {RemotePid, reply, {DestMailbox, Msg}} ->
      io:format("\n---------------------------------------------\n"),
      io:format("[<] Received response from process ~p\n", [RemotePid]),
      io:format("[<] Replying to mailbox ~p\n", [DestMailbox]),
      DestMailbox ! Msg,
      master_loop(Count);

    {'EXIT', FromPid, normal} ->
      io:format("[I] Spawnd process ~p exited successfully\n", [FromPid]),
      master_loop(Count);

    {'EXIT', FromPid, noconnection} ->
      io:format("[E] Spawnd process ~p exited unexpectedly: job lost\n", [FromPid]),
      master_loop(Count);

    _ -> error
  end.

read_config(File) ->
  {ok, Bin} = file:read_file(File),
  String = binary:bin_to_list(Bin),
  Nodes = string:split(String, "\n", all),
  nodes_to_atoms(Nodes).

nodes_to_atoms(Nodes) ->
  nodes_to_atoms(Nodes, []).

nodes_to_atoms([], Atoms) -> Atoms;
nodes_to_atoms([H|T], Atoms) ->
  nodes_to_atoms(T, Atoms ++ [list_to_atom(H)]).

connect_to_nodes([]) -> ok;
connect_to_nodes([H|T]) ->
  net_adm:ping(H),
  connect_to_nodes(T).

setup() ->
  Nodes = read_config("nodes.conf"),
  io:format("[I] Configured nodes: ~p\n", [Nodes]),
  connect_to_nodes(Nodes),
  io:format("[I] Active nodes: ~p\n", [nodes()]).

start() -> start(master).
start(Name) ->
  Pid = spawn(node(), fun() -> master_setup() end),
  register(Name, Pid).