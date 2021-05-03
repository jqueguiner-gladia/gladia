-module(openapi_us_api).

-export([read_user_image_image_uncolorization_users_username_post/2, read_user_image_image_uncolorization_users_username_post/3]).

-define(BASE_URL, "").

%% @doc Read User
%% 
-spec read_user_image_image_uncolorization_users_username_post(ctx:ctx(), binary()) -> {ok, maps:map(), openapi_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), openapi_utils:response_info()}.
read_user_image_image_uncolorization_users_username_post(Ctx, Username) ->
    read_user_image_image_uncolorization_users_username_post(Ctx, Username, #{}).

-spec read_user_image_image_uncolorization_users_username_post(ctx:ctx(), binary(), maps:map()) -> {ok, maps:map(), openapi_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), openapi_utils:response_info()}.
read_user_image_image_uncolorization_users_username_post(Ctx, Username, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = post,
    Path = ["/image/image/uncolorization/users/", Username, ""],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = openapi_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    openapi_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


