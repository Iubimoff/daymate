-module(daymate_db).

-export([open/0]).
-export([insert/2, select/2]).

open() ->
    {ok, Path} = application:get_env(daymate, db_file),
    case esqlite3:open(Path) of
        {ok, Db} ->
            {ok, Db};
        _ ->
            {error, error_while_connect_to_db}
    end.

insert(Query, Params) ->
    {ok, Db} = open(),
    _ = esqlite3:q(Db, Query, Params),
    close(Db).

select(Query, Params) ->
    {ok, Db} = open(),
    Result = esqlite3:q(Db, Query, Params),
    ok = close(Db),
    Result.

close(Db) ->
    esqlite3:close(Db).