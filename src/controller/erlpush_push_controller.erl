-module(erlpush_push_controller, [Req, SessionID]).
-compile(export_all).

broadcast('GET', [AppId], ExtraInfo) ->
    {user_id, UserId} = ExtraInfo,
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
    ex_apns:start(),
    CertFile = filename:join([code:priv_dir(erlpush), "certs", App:id() ++ ".pem"]),
    case ex_apns:start_link('apns_sender', development, CertFile) of
        {ok, ApnsPid} ->
            send_notification(ApnsPid, App:device_tokens(), Payload);
        {error, {already_started, ApnsPid}} ->
            send_notification(ApnsPid, App:device_tokens(), Payload);
        {error, Reason} ->
            error_logger:error_report(Reason),
            boss_flash:add(SessionID, error, "Error sending message", ""),
            error
    end,
    {redirect, [{controller, "applications"}, {action, "show"}, {id, AppId}]}.

before_(_ActionName) ->
    user_utils:require_login(SessionID).

send_notification(ApnsPid, DeviceTokens, Payload) ->
    lists:map(fun(Token) -> ex_apns:send(ApnsPid, Token:value(), Payload) end, DeviceTokens),
    boss_flash:add(SessionID, notice, "Message sent", "").