-module(fixture).
-include_lib("eunit/include/eunit.hrl").

key_between_single_entry_test() ->
	true = key:between( 55, 0,0 ),
	true = key:between(0,0,0).
	
key_between_normal_ring_test() ->
	true = key:between( 15, 1, 16 ),
	false = key:between( 18, 1, 16 ),
	false = key:between (-3,1,14).

key_between_reverse_ring_test() ->
	true = key:between( 200, 50, 26 ),
	false = key:between( 40, 55, 20 ),
	true = key: between( 1, 10, 3 ).
	
key_between_edge_cases_test() ->
	false = key:between( 4 , 5, 10),
	false = key:between( 5 , 5, 10),
	true = key:between( 6 , 5, 10),
	true = key:between( 7 , 5, 10),
	true = key:between( 8 , 5, 10),
	true = key:between( 9, 5, 10),
	true = key:between( 10 , 5, 10),
	false = key:between( 11 , 5, 10),
	
	true = key:between( 11 , 10, 5),
	false = key:between( 10 , 10, 5),
	false = key:between( 9 , 10, 5),
	false = key:between( 8 , 10, 5),
	false = key:between( 7 , 10, 5),
	false = key:between( 6 , 10, 5),
	true = key:between( 5 , 10, 5),
	true = key:between( 4 , 10, 5).

	
stabilize_simplest_circle_case_test() ->
	clear(),
	Me = self(),
	{1,Me} = node:stabilize( {0,Me}, 0, {1,Me} ),
	receive
		{notify, {Key,Peer}} ->
			throw(msg);
		{request, Peer} ->
			throw(msg)
		after 100 ->
			ok
	end.

stabilize_single_circle_test() ->
	clear(),
	Me = self(),
	{1,Me} = node:stabilize( {1,Me}, 0, {1,Me} ),
	receive
		{notify, {Key,Peer}} ->
			Key = 0;
		{request, Peer} ->
			throw(msg)
		after 1000 ->
			throw(msg)
	end.

stabilize_nil_circle_test() ->
	clear(),
	Me = self(),
	{1,Me} = node:stabilize( {1,Me}, 0, {1,Me} ),
	receive
		{notify, {Key,Peer}} ->
			Key = 0;
		{request, Peer} ->
			throw(msg)
		after 1000 ->
			throw(msg)
	end.
	

stabilize_entering_circle_behind_pred_test() ->
	clear(),
	Me = self(),
	{1,Me} = node:stabilize( {1,Me}, 0, {10,Me} ),
	receive
		{notify, {Key,Peer}} ->
			throw(notify);
		{request, Peer} ->
			Peer = Me
		after 1000 ->
			throw(timeout)
	end.
	
	
	
stabilize_entering_circle_between_pred_test() ->
	clear(),
	Me = self(),
	{10,Me} = node:stabilize( {0,Me}, 5, {10,Me} ),
	receive
		{notify, {Key,Peer}} ->
			5=Key;
		{request, Peer} ->
			throw(request)
		after 1000 ->
			throw(timeout)
	end.
	
clear() ->
	receive
		_ -> clear()
		after 100 -> ok
	end.
	
receivednotify_handles_fake_message_test() ->
	{4,id} = node:received_notify({8,id},5,{4,id}).
	
receivednotify_handles_init_message_test() ->
	{8,id} = node:received_notify({8,id},5,nil).
	
receivednotify_handles_wrapping_message_test() ->
	{8,id} = node:received_notify({8,id},3,{4,id}).
