-module(openapi_api).

-export([ read_user_me_image_image_uncolorization_users_me_get/0
        ]).

-define(BASE_URL, "").

%% @doc Read User Me
%% 
-spec read_user_me_image_image_uncolorization_users_me_get() ->
  openapi_utils:response().
read_user_me_image_image_uncolorization_users_me_get() ->
  Method      = get,
  Host        = application:get_env(openapi, host, "http://localhost:8080"),
  Path        = ["/image/image/uncolorization/users/me"],

  openapi_utils:request(Method, [Host, ?BASE_URL, Path]).

