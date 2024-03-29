-module(erlpush_applications_controller, [Req, SessionID]).
-compile(export_all).

-default_action(index).

index('GET', [], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    Apps = boss_db:find(app, [push_user_id = UserId]),
    {ok, [{apps, Apps}]}.

show('GET', [Id], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    find_app(Id, UserId).

create('GET', [], ExtraInfo) ->
    ok;
create('POST', [], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    Attributes = params_to_proplist(),
    NewApp = boss_record:new(app, [{push_user_id, UserId}] ++ Attributes),
    case NewApp:save() of
        {ok, SavedApp} ->
            case rename_certfile(SavedApp, Req:post_files()) of
                ok ->
                    boss_flash:add(SessionID, notice, "Application successfully created", ""),
                    {redirect, [{action, "index"}]};
                {error, Reason} ->
                    boss_flash:add(SessionID, error, "Error creating application", "cannot rename certificate file"),
                    {redirect, [{action, "create"}]}
            end;
        {error, ErrorList} ->
            lists:foreach(fun(Error) -> boss_flash:add(SessionID, error, "Error creating application", Error) end, ErrorList),
            {redirect, [{action, "create"}]}
    end.

edit('GET', [Id], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    find_app(Id, UserId);
edit('POST', [Id], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    case find_app(Id, UserId) of
        {ok, Params} ->
            edit_app(proplists:get_value(app, Params));
        not_found ->
            not_found;
        {error, Reason} ->
            {error, "Error editing app", []}
    end.

before_(_ActionName) ->
    user_utils:require_login(SessionID, Req:uri()).

find_app(Id, UserId) ->
    case boss_db:find(Id) of
        {error, Reason} ->
            error_logger:error_msg("Error finding app with reason: ~p~n", [Reason]),
            {error, "Error finding app", []};
        App ->
            case App:push_user_id() of
                UserId ->
                    Events = boss_db:find(event, [app_id = App:id()], 10, 0, creation_time, num_descending),
                    {ok, [{app, App}, {events, Events}]};
                _ -> not_found
            end
    end.

edit_app(undefined) ->
    error_logger:error_msg("Error editing app because app is undefined~n"),
    {error, "Error editing app", []};
edit_app(App) ->
    NewAttributes = params_to_proplist(),
    NewApp = App:set(NewAttributes),
    case NewApp:save() of
        {ok, SavedApp} ->
            case rename_certfile(SavedApp, Req:post_files()) of
                ok ->
                    boss_flash:add(SessionID, notice, "Application successfully updated", ""),
                    {redirect, [{action, "show"}, {id, App:id()}]};
                {error, Reason} ->
                    boss_flash:add(SessionID, error, "Error updating application", "cannot rename certificate file"),
                    {redirect, [{action, "edit"}, {id, App:id()}]}
            end;
        {error, ErrorList} ->
            lists:foreach(fun(Error) -> boss_flash:add(SessionID, error, "Error updating application", Error) end, ErrorList),
            {redirect, [{action, "edit"}, {id, App:id()}]}
    end.

params_to_proplist() ->
    Name = Req:post_param("name"),
    AppMode = Req:post_param("app_mode"),
    DebugMode = case Req:post_param("debug_mode") of
        "1" -> true;
        "on" -> true;
        _   -> false
    end,
    GcmApiKey = Req:post_param("gcm_api_key"),
    [{name, Name}, {app_mode, AppMode}, {debug_mode, DebugMode}, {gcm_api_key, GcmApiKey}].

rename_certfile(App, []) ->
    ok;
rename_certfile(App, [CertFile]) ->
    {uploaded_file, _FileName, TempPath, _FileSize} = CertFile,
    CertFileName = "priv/certs/" ++ App:id() ++ ".pem",
    case file:rename(TempPath, CertFileName) of
        ok -> ok;
        {error, Reason} ->
            error_logger:error_msg("Error renaming cert file: ~p~n", [Reason]),
            boss_db:delete(App:id()),
            {error, Reason}
    end.