-module(hlc_server).
-include_lib("hlc_server/include/hlc_server_pb.hrl").
-behaviour(gen_server).

-export([start_link/0]).
-export([get_clock/0, receive_clock/1, to_int/1, get_diff_ns/2, from_int/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-record(state, {hlc_clock}).

%% If an event is 500000 (500 ms) microseconds in the future, then tell it to fuck right off.
-define(MAX_OUT_OF_BOUNDS, 500000). 
start_link() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
	% TODO: Add code for syncronization amongst other nodes for bootstrap
	{ok, _Pid} = pg2:start(),
	ok = timer:start(),
	pg2:create(hlc_server),
	random:seed(os:timestamp()),
	case hlc_server_time:pt() of 
		{ok, Timestamp} ->
			State = #state{hlc_clock = #hlc_clock{logical_clock = Timestamp}},
			timer:send_interval(1000 + random:uniform(5000), tick), 
			NewState = tick(State),
			pg2:join(hlc_server, self()),
			{ok, NewState};
		Error ->
			{stop, {bad_clock, Error}}
	end.


bootstrap() ->
	% This should move to a gossip mechanism... or something less terrible than O(N**2)
	Members = pg2:get_members(hlc_server) -- [self()],
	[gen_server:cast(Member, {bootstrap, self()}) || Member <- Members],
	ok.

timestamp() ->
	case hlc_server_time:pt() of
		{ok, Timestamp} ->
			Timestamp;
		%% This only works, because wherever we're taking the value of timestamp
		%% we're also comparing it to the last clock we saw. 
		_Else ->
			0 
	end.

handle_cast({bootstrap, RemotePid}, State) ->
	NewClock = get_clock(State),
	gen_server:cast(RemotePid, {update_clock, NewClock}),
	NewState = State#state{hlc_clock = NewClock},
	{noreply, NewState};
handle_cast({update_clock, RemoteClock}, State) ->
	NewClock = receive_clock(RemoteClock, State),
	NewState = State#state{hlc_clock = NewClock},
	{noreply, NewState};
handle_cast({broadcast_clock, RemoteClock}, State) ->
	NewClock = receive_clock(RemoteClock, State),
	NewState = State#state{hlc_clock = NewClock},
	{noreply, NewState};
handle_cast(Stuff, State) ->
	io:format("Got unknown handle_cast: ~p~n", [Stuff]),
	{noreply, State}.

handle_call(get_clock, _From, State) ->
	NewClock = get_clock(State),
	NewState = State#state{hlc_clock = NewClock},
	{reply, NewClock, NewState};
	
handle_call({receive_clock, RemoteClock}, _From, State) ->
	NewClock = receive_clock(RemoteClock, State),
	NewState = State#state{hlc_clock = NewClock},
	{reply, NewClock, NewState}.

terminate(_Reason, _State) ->
	io:format("HLC Server terminating~n").
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(tick, State) ->
	NewState = tick(State),
	{noreply, NewState};
handle_info(_Info, State) ->
	{noreply, State}.

tick(State) ->
	bootstrap(),
	NewClock = get_clock(State),
	NewState = State#state{hlc_clock = NewClock},
	NewState.

get_clock(_State = #state{hlc_clock = Clock}) ->
	LogicalTime = erlang:max(Clock#hlc_clock.logical_clock, timestamp()),
	CurrentLogicalTime = Clock#hlc_clock.logical_clock,
	case LogicalTime of 
		CurrentLogicalTime ->
			NewClock = #hlc_clock{logical_clock = LogicalTime, event_clock = Clock#hlc_clock.event_clock + 1};
		_ ->
			NewClock = #hlc_clock{logical_clock = LogicalTime, event_clock = 0}
	end,
	broadcast_clock(NewClock),
	NewClock.

broadcast_clock(Clock) ->
	% We should add throttling here to only make it happen once in a while
	% Otherwise every event results in a cross-cluster message
	% Which resulted in hilarious O(N**2) network overhead per clock event... lol
	Servers = pg2:get_members(hlc_server) -- [self()],
	[gen_server:cast(Server, {broadcast_clock, Clock}) || Server <- Servers].

receive_clock(RemoteClock, _State = #state{hlc_clock = Clock}) ->
	CurrentLogicalTime = Clock#hlc_clock.logical_clock,
	LocalLogicalTime = erlang:max(Clock#hlc_clock.logical_clock, timestamp()),
	HighestLogicalTime = erlang:max(LocalLogicalTime, RemoteClock#hlc_clock.logical_clock),
	RemoteLogicalTime = RemoteClock#hlc_clock.logical_clock,
	if 
		HighestLogicalTime =:= CurrentLogicalTime andalso HighestLogicalTime =:= RemoteLogicalTime  ->
			EventClock = erlang:max(RemoteClock#hlc_clock.event_clock, Clock#hlc_clock.event_clock) + 1;
		LocalLogicalTime =:= CurrentLogicalTime ->
			EventClock = Clock#hlc_clock.event_clock + 1;
		HighestLogicalTime =:= RemoteLogicalTime ->
			EventClock = Clock#hlc_clock.event_clock + 1;
		true ->
			EventClock = 0
	end,
	NewClock = #hlc_clock{logical_clock = HighestLogicalTime, event_clock = EventClock},
	NewClock.

get_clock() ->
	gen_server:call(?MODULE, get_clock).
receive_clock(Clock) ->
	gen_server:call(?MODULE, {receive_clock, Clock}).

to_int(Clock) ->
	LogicalClock = Clock#hlc_clock.logical_clock,
	EventClock = Clock#hlc_clock.event_clock,
	<<IntClock:96/unsigned-integer>> = <<LogicalClock:64/unsigned-integer, EventClock:32/unsigned-integer>>,
	IntClock.

from_int(IntClock) ->
	<<LogicalClock:64/unsigned-integer, EventClock:32/unsigned-integer>> = <<IntClock:96/unsigned-integer>>,
	#hlc_clock{logical_clock = LogicalClock, event_clock = EventClock}.


get_diff_ns(Clock1, Clock2) ->
	% Clock2 occurs at least N nanoseconds after Clock1.
	Clock2#hlc_clock.logical_clock - Clock1#hlc_clock.logical_clock.
