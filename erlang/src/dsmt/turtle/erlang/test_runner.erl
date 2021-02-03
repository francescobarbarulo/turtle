-module(test_runner).

-export([start/2, alert_master/0]).

start(Master, {Mailbox, Msg}) ->
  % io:format("[{~p, ~s}] New process spawned for client ~p\n", [self(), node(), Mailbox]),
  Result = process(Msg),
  % io:format("[{~p, ~s}] Sending response to master_node\n", [self(), node()]),
  % timer:sleep(10000), % to simulate a slower machine
  Master ! {self(), response, {Mailbox, Result}}.

process({Session, {ClassFileName, ClassFileContent, TestClassFileName, TestClassFileContent}}) ->
  % io:format("[{~p, ~s}] Started processing job for ~s\n", [self(), node(), Session]),
  PidString = pid_to_list(self()),
  PidList = string:split(PidString, ".", all),
  Pid = lists:nth(2, PidList),

  % Create working directory and saving files
  CurrentDir = "/tmp/turtle/" ++ Session ++ "." ++ Pid ++ "/",
  % io:format("[{~p, ~s}] Creating workspace: ~s\n", [self(), node(), CurrentDir]),
  file:make_dir(CurrentDir),
  file:write_file(CurrentDir ++ ClassFileName, ClassFileContent),
  file:write_file(CurrentDir ++ TestClassFileName, TestClassFileContent),

  % Compile
  CompileCmd = "javac -cp /tmp/turtle/junit-4.13.1.jar " ++ CurrentDir ++ ClassFileName ++ " " ++ CurrentDir ++ TestClassFileName,
  os:cmd(CompileCmd),
  % io:format("[{~p, ~s}] ~s\n", [self(), node(), CompileOut]),

  % Run test
  RunCmd = "java -cp " ++ CurrentDir ++ ":/tmp/turtle/junit-4.13.1.jar:/tmp/turtle/hamcrest-core-1.3.jar org.junit.runner.JUnitCore " ++ lists:nth(1, string:tokens(TestClassFileName, ".")),
  RunOut = os:cmd(RunCmd),
  % io:format("[{~p, ~s}] ~s\n", [self(), node(), RunOut]),

  % Clean workspace
  file:del_dir_r(CurrentDir),
  RunOut.

alert_master() ->
  net_adm:ping(master_node@master).