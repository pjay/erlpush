-module(device_token, [Id, AppId::string(), Value::string(), LastRegistrationTime::datetime()]).
-compile(export_all).

-belongs_to(app).

validation_tests() ->
	[{fun() -> length(Value) =:= 64 end, "Device token should have 64 characters"}].