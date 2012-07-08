-module(erlpush_push_controller, [Req, SessionID]).
-compile(export_all).

broadcast('GET', [AppId], ExtraInfo) ->
    UserId = proplists:get_value(user_id, ExtraInfo),
    case boss_db:find(AppId) of
        undefined ->
            not_found;
        App ->
            case App:push_user_id() of
                UserId -> {ok, [{user_id, UserId}, {app, App}]};
                _ -> not_found
            end;
        {error, Reason} ->
            not_found
    end;
broadcast('POST', [AppId], ExtraInfo) ->
    App = boss_db:find(AppId),
    Message = Req:post_param("message"),
    Payload = [{aps,[{alert, list_to_binary(Message)}]}],
    push_dispatcher:start(),
    push_dispatcher:send_broadcast(App, Payload),
    {redirect, [{controller, "applications"}, {action, "show"}, {id, AppId}]}.

before_(_ActionName) ->
    user_utils:require_login(SessionID, Req:uri()).