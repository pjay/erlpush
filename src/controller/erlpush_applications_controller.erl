-module(erlpush_applications_controller, [Req, SessionID]).
-compile(export_all).

-default_action(index).

index('GET', [], ExtraInfo) ->
    {user_id, UserId} = ExtraInfo,
    Applications = boss_db:find(mobile_application, [push_user_id = UserId]),
    {ok, [{applications, Applications}]}.

show('GET', [Id], ExtraInfo) ->
    {user_id, UserId} = ExtraInfo,
    case boss_db:find(Id) of
        Application ->
            case Application:push_user_id() of
                UserId -> {ok, [{mobile_application, Application}]};
                _ -> not_found
            end;
        {error, Reason} ->
            not_found
    end.

create('GET', [], ExtraInfo) ->
    ok;
create('POST', [], ExtraInfo) ->
    {user_id, UserId} = ExtraInfo,
    Name = Req:post_param("name"),
    CertFile = Req:post_files(),
    [{uploaded_file, _FileName, TempPath, _FileSize}] = CertFile,
    DebugMode = case Req:post_param("debug_mode") of
        "1" -> true;
        _   -> false
    end,
    NewApplication = boss_record:new(mobile_application, [{push_user_id, UserId}, {name, Name}, {debug_mode, DebugMode}]),
    case NewApplication:save() of
        {ok, SavedApplication} ->
            CertFileName = "priv/certs/" ++ SavedApplication:id() ++ ".pem",
            file:rename(TempPath, CertFileName),
            boss_flash:add(SessionID, notice, "Application successfully created", ""),
            {redirect, [{action, "index"}]};
        {error, ErrorList} ->
            lists:foreach(fun(Error) -> boss_flash:add(SessionID, error, "Error creating user", Error) end, ErrorList),
            {redirect, [{action, "create"}]}
    end.

before_(_ActionName) ->
    user_utils:require_login(SessionID).