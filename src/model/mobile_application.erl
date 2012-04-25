-module(mobile_application, [Id, PushUserId::string(), Name::string(), ApiKey::string(), ApiSecret::string(), DebugMode::boolean()]).
-compile(export_all).

-belongs_to(push_user).
-has({device_tokens, many}).

before_create() ->
    NewKey = mobile_application_utils:generate_key(),
    NewSecret = mobile_application_utils:generate_secret(),
    ModifiedRecord = set([{api_key, NewKey}, {api_secret, NewSecret}]),
    {ok, ModifiedRecord}.

validation_tests() ->
    [{fun() -> length(Name) > 0 end, "Name is mandatory"}].