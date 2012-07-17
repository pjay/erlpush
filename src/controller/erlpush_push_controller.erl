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
    push_dispatcher:start(),
    case Req:post_param("type") of
        "ios" ->
            push_dispatcher:send_broadcast_ios(App, broadcast_payload_ios()),
            boss_flash:add(SessionID, notice, "The broadcast notification is being sent", ""),
            {redirect, [{controller, "applications"}, {action, "show"}, {id, AppId}]};
        "gcm" ->
            push_dispatcher:send_broadcast_gcm(App, broadcast_payload_gcm()),
            boss_flash:add(SessionID, notice, "The broadcast notification is being sent", ""),
            {redirect, [{controller, "applications"}, {action, "show"}, {id, AppId}]};
        _ ->
            boss_flash:add(SessionID, error, "Bad request", "the 'type' field doesn't contain a valid value"),
            {redirect, [{controller, "push"}, {action, "broadcast"}, {id, AppId}]}
    end.

before_(_ActionName) ->
    user_utils:require_login(SessionID, Req:uri()).

broadcast_payload_ios() ->
    Alert = Req:post_param("alert"),
    [{<<"aps">>,[{<<"alert">>, list_to_binary(Alert)}]}].

broadcast_payload_gcm() ->
    [{<<"message">>,list_to_binary(<<"test">>)}].