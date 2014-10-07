-ifndef(HLC_CLOCK_PB_H).
-define(HLC_CLOCK_PB_H, true).
-record(hlc_clock, {
    logical_clock = 0,
    event_clock = 0
}).
-endif.

