-module(device_token, [Id, MobileApplicationId::string(), Value::string(), LastRegistrationTime::datetime()]).
-compile(export_all).

-belongs_to(mobile_application).