-module(mobile_application_utils).
-compile(export_all).

generate_key() ->
    <<Salt:160/integer>> = crypto:rand_bytes(20),
    [Hex|_] = io_lib:format("~40.16.0b", [Salt]),
    Hex.

generate_secret() ->
	<<Salt:160/integer>> = crypto:rand_bytes(20),
    [Hex|_] = io_lib:format("~40.16.0b", [Salt]),
    Hex.