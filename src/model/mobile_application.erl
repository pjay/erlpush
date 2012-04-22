-module(mobile_application, [Id, PushUserId::string(), Name::string(), DebugMode::boolean()]).
-compile(export_all).

-belongs_to(push_user).

validation_tests() ->
    [{fun() -> length(Name) > 0 end, "Name is mandatory"}].