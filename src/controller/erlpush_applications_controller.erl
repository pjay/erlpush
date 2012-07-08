-module(erlpush_applications_controller, [Req, SessionID]).
-compile(export_all).

-default_action(index).

index('GET', [], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    Apps = boss_db:find(app, [push_user_id = UserId]),
    {ok, [{apps, Apps}]}.

show('GET', [Id], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    case boss_db:find(Id) of
        undefined ->
            not_found;
        App ->
            case App:push_user_id() of
                UserId -> {ok, [{app, App}]};
                _ -> not_found
            end;
        {error, Reason} ->
            not_found
    end.

create('GET', [], ExtraInfo) ->
    ok;
create('POST', [], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    Name = Req:post_param("name"),
    CertFile = Req:post_files(),
    [{uploaded_file, _FileName, TempPath, _FileSize}] = CertFile,
    DebugMode = case Req:post_param("debug_mode") of
        "1" -> true;
        _   -> false
    end,
    NewApp = boss_record:new(app, [{push_user_id, UserId}, {name, Name}, {debug_mode, DebugMode}]),
    case NewApp:save() of
        {ok, SavedApp} ->
            CertFileName = "priv/certs/" ++ SavedApp:id() ++ ".pem",
            file:rename(TempPath, CertFileName),
            boss_flash:add(SessionID, notice, "Application successfully created", ""),
            {redirect, [{action, "index"}]};
        {error, ErrorList} ->
            lists:foreach(fun(Error) -> boss_flash:add(SessionID, error, "Error creating user", Error) end, ErrorList),
            {redirect, [{action, "create"}]}
    end.

before_(_ActionName) ->
    user_utils:require_login(SessionID).