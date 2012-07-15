-module(app, [Id, PushUserId::string(), Name::string(), ApiKey::string(), ApiSecret::string(), MasterSecret::string(), AppMode::string(), DebugMode::boolean()]).
-compile(export_all).

-belongs_to(push_user).
-has({device_tokens, many}).
-has({registrations, many}).
-has({notifications, many}).
-has({events, many, [{sort_by, creation_time}, {sort_order, num_descending}]}).

cert_path() ->
    filename:join([code:priv_dir(erlpush), "certs", Id ++ ".pem"]).

before_create() ->
    NewKey = app_utils:generate_key(),
    NewSecret = app_utils:generate_secret(),
    NewMasterSecret = app_utils:generate_secret(),
    ModifiedRecord = set([{api_key, NewKey}, {api_secret, NewSecret}, {master_secret, MasterSecret}]),
    {ok, ModifiedRecord}.

validation_tests() ->
    [{fun() -> length(Name) > 0 end, "Name is mandatory"},
     {fun() -> AppMode =:= "development" orelse AppMode =:= "production" end, "Application mode should be development or production"}].