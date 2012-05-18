-module(erlpush_push_controller, [Req, SessionID]).
-compile(export_all).

broadcast('GET', [AppId], ExtraInfo) ->
    {user_id, UserId} = ExtraInfo,
    case boss_db:find(AppId) of
        Application ->
            case Application:push_user_id() of
                UserId -> {ok, [{user_id, UserId}, {application, Application}]};
                _ -> not_found
            end;
        {error, Reason} ->
            not_found
    end;
broadcast('POST', [AppId], ExtraInfo) ->
    %% TODO
    boss_flash:add(SessionID, notice, "Message sent", ""),
    {redirect, [{action, "broadcast"}, {app_id, AppId}]}.

before_(_ActionName) ->
    user_utils:require_login(SessionID).