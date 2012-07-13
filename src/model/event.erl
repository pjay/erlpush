-module(event, [Id, AppId::string(), CreationTime::datetime(), Severity::integer(), Message::string()]).

-export([severity_string/0]).
-export([validation_tests/0]).

-belongs_to(app).

-define(CRITICAL, 0).
-define(ERROR, 1).
-define(WARNING, 2).
-define(INFO, 3).
-define(DEBUG, 4).

severity_string() when Severity =:= ?CRITICAL ->
    "critical";
severity_string() when Severity =:= ?ERROR ->
    "error";
severity_string() when Severity =:= ?WARNING ->
    "warning";
severity_string() when Severity =:= ?INFO ->
    "info";
severity_string() when Severity =:= ?DEBUG ->
    "debug".

validation_tests() ->
    [{fun() -> length(Message) > 0 end, "Message is mandatory"},
     {fun() -> lists:member(Severity, [?CRITICAL, ?ERROR, ?WARNING, ?INFO, ?DEBUG]) end, "Invalid severity"}].