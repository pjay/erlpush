-module(device_token, [Id, MobileApplicationId::string(), Value::string(), LastRegistrationTime::datetime()]).
-compile(export_all).

-belongs_to(mobile_application).

validation_tests() ->
	[{fun() -> length(Value) =:= 64 end, "Device token should have 64 characters"}].