-module(test_runner).

-export([start/2, alert_master/0]).

start(Master, {Mailbox, Msg}) ->
  io:format("[~s] New process spawned for client ~p\n", [node(), Mailbox]),
  Result = process(Msg),
  io:format("[~s] Replying to master_node\n", [node()]),
  % timer:sleep(10000), % to simulate a slower machine
  Master ! {self(), reply, {Mailbox, Result}}.

process({Session, {ClassFileName, ClassFileContent, TestClassFileName, TestClassFileContent}}) ->
  io:format("Started processing job for ~s\n", [Session]),
  PidString = pid_to_list(self()),
  PidList = string:split(PidString, ".", all),
  Pid = lists:nth(2, PidList),

  % Create working directory and saving files
  CurrentDir = "/tmp/turtle/" ++ Session ++ "." ++ Pid ++ "/",
  io:format("Creating workspace: ~s\n", [CurrentDir]),
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

alert_master() ->
  net_adm:ping(master_node@master).