-module(hlc_server_time).
-on_load(init/0).
-export([pt/0]).
pt() ->
  erlang:nif_error({error, not_loaded}).

init() ->
    case code:priv_dir(hlc_server) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    SoName = filename:join([filename:dirname(Filename),"../priv", "hlc_server"]);
                _ ->
                    SoName = filename:join("../priv", "hlc_server")
            end;
        Dir ->
            SoName = filename:join(Dir, "hlc_server")
    end,
    erlang:load_nif(SoName, 0).

