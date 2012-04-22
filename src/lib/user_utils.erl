-module(user_utils).
-compile(export_all).

generate_hex_salt() ->
    <<Salt:160/integer>> = crypto:rand_bytes(20),
    [Hex|_] = io_lib:format("~40.16.0b", [Salt]),
    Hex.

sha1_hex(Salt, Password) ->
    <<HashedPassword:160/integer>> = crypto:sha(Salt ++ Password),
    [Hex|_] = io_lib:format("~40.16.0b", [HashedPassword]),
    Hex.

require_login(SessionID) ->
    case boss_session:get_session_data(SessionID, "user_id") of
        UserId when is_list(UserId) ->
            {ok, {user_id, UserId}};
        _ ->
            {redirect, [{controller, "users"}, {action, "login"}]}
    end.