-module(test_runner).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, test/1]).

start() ->
  gen_server:start({local, test_runner}, ?MODULE, [], []).

test(Request) ->
  gen_server:call(test_runner, Request).

process(Session, ClassFileName, ClassFileContent, TestClassFileName, TestClassFileContent) ->
  io:format("Started processing job for ~s\n", [Session]),

  % Create working directory and saving files
  CurrentDir = "/tmp/turtle/" ++ Session ++ "/",
  file:make_dir(CurrentDir),
  file:write_file(CurrentDir ++ ClassFileName, ClassFileContent),
  file:write_file(CurrentDir ++ TestClassFileName, TestClassFileContent),

  % Compile
  CompileCmd = "javac -cp /tmp/turtle/junit-4.13.1.jar " ++ CurrentDir ++ ClassFileName ++ " " ++ CurrentDir ++ TestClassFileName,
  CompileOut = os:cmd(CompileCmd),
  io:format("~s\n", [CompileOut]),

  % Run test
  RunCmd = "java -cp " ++ CurrentDir ++ ":/tmp/turtle/junit-4.13.1.jar:/tmp/turtle/hamcrest-core-1.3.jar org.junit.runner.JUnitCore " ++ lists:nth(1, string:tokens(TestClassFileName, ".")),
  RunOut = os:cmd(RunCmd),
  io:format("~s\n", [RunOut]),

  % Clean workspace
  file:del_dir_r(CurrentDir),
  RunOut.

init(_Args) ->
  {ok, {}}.

handle_call({Session, {ClassFileName, ClassFileContent, TestClassFileName, TestClassFileContent}}, _From, State) ->
  Reply = process(Session, ClassFileName, ClassFileContent, TestClassFileName, TestClassFileContent),
  {reply, Reply, State}.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).