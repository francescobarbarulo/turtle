-module(master_node).

%% API
-export([start/0, stop/0]).

master_setup() ->
  process_flag(trap_exit, true),
  setup(maps:new()).

master_loop(Count, Jobs) ->
  io:format("-------------------------------------------------------\n"),
  io:format("[*] Waiting for a message...\n"),
  receive
    {FromMailbox, Msg} ->
      try (Count rem length(nodes())) + 1 of
        Index ->
          TargetNode = lists:nth(Index, nodes()),
          io:format(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"),
          io:format("[>] Received new job from ~p\n", [FromMailbox]),
          io:format("[>] Spawning new process on node ~p\n", [TargetNode]),
          Master = self(),
          Pid = spawn_link(TargetNode, test_runner, start, [Master, {FromMailbox, Msg}]),
          master_loop(Count+1, maps:put(Pid, {FromMailbox, Msg}, Jobs))
      catch error:badarith ->
        io:format("[W] All nodes are down\n"),
        self() ! {FromMailbox, Msg},
        timer:sleep(2000),
        setup(Jobs)
      end;

    {RemotePid, response, {DestMailbox, Msg}} ->
      io:format("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n"),
      io:format("[<] Received response from process ~p\n", [RemotePid]),
      io:format("[<] Replying to mailbox ~p\n", [DestMailbox]),
      DestMailbox ! Msg,
      master_loop(Count, maps:remove(RemotePid, Jobs));

    {'EXIT', FromPid, normal} ->
      io:format("[I] Spawnd process ~p exited successfully\n", [FromPid]),
      master_loop(Count, Jobs);

    {'EXIT', FromPid, noconnection} ->
      io:format("[E] Spawnd process ~p exited unexpectedly: trying to recover\n", [FromPid]),
      Request = maps:get(FromPid, Jobs),
      self() ! Request,
      master_loop(Count, maps:remove(FromPid, Jobs));

    stop ->
      io:format("[I] Terminating...\n");

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
  nodes_to_atoms(T,  [list_to_atom(H)|Atoms]).

connect_to_nodes([]) -> ok;
connect_to_nodes([H|T]) ->
  net_adm:ping(H),
  connect_to_nodes(T).

setup(Jobs) ->
  io:format("[I] Setup\n"),
  Nodes = read_config("nodes.conf"),
  io:format("[I] Configured nodes: ~p\n", [Nodes]),
  connect_to_nodes(Nodes),
  io:format("[I] Active nodes: ~p\n", [nodes()]),
  master_loop(0, Jobs).

start() -> start(master).
start(Name) ->
  Pid = spawn(node(), fun() -> master_setup() end),
  register(Name, Pid).

stop() ->
  {master, node()} ! stop.