-module(erlpush_users_controller, [Req, SessionID]).
-compile(export_all).

-default_action(index).

index('GET', []) ->
    {redirect, [{action, "login"}]}.

login('GET', []) ->
    ok;
login('POST', []) ->
    Email = Req:post_param("email"),
    Password = Req:post_param("password"),
    Matches = boss_db:find(push_user, [email, 'equals', Email]),
    case length(Matches) of
        1 ->
            User = hd(Matches),
            HashedPassword = user_utils:sha1_hex(User:salt(), Password),
            case HashedPassword =:= User:hashed_password() of
                true ->
                    boss_session:set_session_data(SessionID, "user_id", User:id()),
                    case boss_session:get_session_data(SessionID, "uri_before_login") of
                        Uri ->
                            boss_session:remove_session_data(SessionID, "uri_before_login"),
                            {redirect, Uri};
                        {error, _Reason} -> {redirect, [{controller, "applications"}]}
                    end;
                false ->
                    boss_flash:add(SessionID, error, "Login failed", "Wrong username or password"),
                    {redirect, [{action, "login"}]}
            end;
        _ ->
            boss_flash:add(SessionID, error, "Login failed", "Wrong username or password"),
            {redirect, [{action, "login"}]}
    end.

logout('GET', []) ->
    boss_session:remove_session_data(SessionID, "user_id"),
    {redirect, [{action, "login"}]}.

create('GET', []) ->
    ok;
create('POST', []) ->
    Email = Req:post_param("email"),
    Password = Req:post_param("password"),
    PasswordConfirmation = Req:post_param("password_confirmation"),

    % Verify that the email is not yet taken
    Matches = boss_db:find(push_user, [email = Email]),
    case length(Matches) of
        1 ->
            boss_flash:add(SessionID, error, "E-mail already taken", "Another user already exists with the same e-mail"),
            {redirect, [{action, "create"}]};
        _ ->
            % Verify that both the password and the password confirmation do match
            case Password =:= PasswordConfirmation of
                true ->
                    % Create the user
                    Salt = user_utils:generate_hex_salt(),
                    HashedPassword = user_utils:sha1_hex(Salt, Password),
                    User = push_user:new(id, Email, Salt, HashedPassword),
                    case User:save() of
                        {ok, NewUser} ->
                            {redirect, [{controller, "applications"}]};
                        {error, ErrorList} ->
                            lists:foreach(fun(Error) -> boss_flash:add(SessionID, error, "Error creating user", Error) end, ErrorList),
                            {redirect, [{action, "create"}]}
                    end;
                false ->
                    boss_flash:add(SessionID, error, "Error creating user", "The password confirmation doesn't match the password"),
                    {redirect, [{action, "create"}]}
            end
    end.