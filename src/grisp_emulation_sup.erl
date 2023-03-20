-module(grisp_emulation_sup).

-behaviour(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, {#{}, [worker(grisp_emulation_device)]}}.

%--- Internal ------------------------------------------------------------------

worker(Module) -> #{id => Module, start => {Module, start_link, []}}.
