-module(push_user, [Id, Email::string(), Salt::string(), HashedPassword::string()]).
-compile(export_all).

-has({mobile_applications, many}).

validation_tests() ->
    [{fun() -> length(Email) > 0 end, "Email is mandatory"}].