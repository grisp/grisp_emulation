-module(grisp_emulation_pmod_maxsonar).

% Callbacks
-export([init/0]).
-export([open/5]).
-export([command/5]).


%--- Macros --------------------------------------------------------------------

-define(UPDATE_INTERVAL, 1000).


%--- Records -----------------------------------------------------------------

-record(state, {
    owner :: pid() | undefined,
    proc :: pid() | undefined
}).


%--- Callbacks -----------------------------------------------------------------

init() ->
    #state{}.

open(State = #state{owner = undefined},
     grisp2, Owner, "grisp_termios_drv", _Settings) ->
    Proc = proc_start(Owner),
    {Proc, State#state{owner = Owner, proc = Proc}}.

command(State = #state{owner = Owner, proc = Proc},
        grisp2, Owner, Proc, _Command) ->
    {ok, State}.


%--- Internal ------------------------------------------------------------------

proc_start(Owner) ->
    spawn_link(fun() -> proc_init(Owner) end).

proc_init(Owner) ->
    self() ! update,
    proc_loop(Owner).

proc_loop(Owner) ->
    receive
        update ->
            Val = rand:uniform(5) + 39,
            D1 = $0,
            D2 = $4,
            D3 = $0 + Val - 40,
            Owner ! {self(), {data, <<$R, D1, D2, D3, $\n>>}},
            erlang:send_after(?UPDATE_INTERVAL, self(), update),
            proc_loop(Owner);
        stop ->
            ok
    end.
