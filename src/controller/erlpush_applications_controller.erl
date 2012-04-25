-module(erlpush_applications_controller, [Req, SessionID]).
-compile(export_all).

-default_action(index).

index('GET', [], ExtraInfo) ->
    {user_id, UserId} = ExtraInfo,
    Applications = boss_db:find(mobile_application, [push_user_id = UserId]),
    {ok, [{applications, Applications}]}.

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

before_(ActionName) ->
    user_utils:require_login(SessionID).