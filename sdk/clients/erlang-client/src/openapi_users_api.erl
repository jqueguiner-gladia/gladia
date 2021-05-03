-module(openapi_users_api).

-export([read_user_me_image_image_uncolorization_users_me_get/1, read_user_me_image_image_uncolorization_users_me_get/2]).

-define(BASE_URL, "").

%% @doc Read User Me
%% 
-spec read_user_me_image_image_uncolorization_users_me_get(ctx:ctx()) -> {ok, maps:map(), openapi_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), openapi_utils:response_info()}.
read_user_me_image_image_uncolorization_users_me_get(Ctx) ->
    read_user_me_image_image_uncolorization_users_me_get(Ctx, #{}).

-spec read_user_me_image_image_uncolorization_users_me_get(ctx:ctx(), maps:map()) -> {ok, maps:map(), openapi_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), openapi_utils:response_info()}.
read_user_me_image_image_uncolorization_users_me_get(Ctx, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = get,
    Path = ["/image/image/uncolorization/users/me"],
    QS = [],
    Headers = [],
    Body1 = [],
    ContentTypeHeader = openapi_utils:select_header_content_type([]),
    Opts = maps:get(hackney_opts, Optional, []),

    openapi_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


