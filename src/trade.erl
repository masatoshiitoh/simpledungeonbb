%%
%%      Copyright (C) 2010 by Masatoshi Itoh
%%      http://www.simpledungeon.com/
%%
%%  This Program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation; either version 2, or (at your option)
%%  any later version.
%%
%%  This Program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with simple dungeon; see the file COPYING.  If not, write to
%%  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
%%  http://www.gnu.org/copyleft/gpl.html
%%


-module(trade).
-compile(export_all).

-include_lib("mmoasp.hrl").
-include_lib("stdlib/include/qlc.hrl").

db_start_trade(Cid1, Cid2) ->
	CidPair = {cid_pair, CidL, CidH} = u:cid_pair(Cid1, Cid2),
	F = fun() ->
		set_tid_or_abort(CidL, void, CidPair),
		set_tid_or_abort(CidH, void, CidPair),
		add_new_trade_row(CidPair)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} -> {ok, CidPair};
		Other -> {ng, Other}
	end.

db_set_offer(Cid, Money, Supplies, Estates) ->
	F = fun(X) ->
		money_offer(Cid, Money),
		estate_offer(Cid, Estates),
		supplies_offer(Cid, Supplies),
		reset_trade_confirm_flag(X)
	end,
	lookup_cidpair_and_do_transaction(Cid, F).

db_get_offer(Cid) ->
	F = fun({cid_pair, Cid1, Cid2}) ->
		Offer1 = get_offer_1(Cid1),
		Offer2 = get_offer_1(Cid2),
		[Offer1, Offer2]
	end,
	lookup_cidpair_and_do_transaction(Cid, F).

db_cancel_trade(Cid) ->
	F = fun(X) ->
		revert_offer(X),
		delete_trade(X)
	end,
	lookup_cidpair_and_do_transaction(Cid, F).

db_confirm_trade(Cid) ->
	F = fun(X) ->
		set_trade_confirm_flag(X, Cid),
		IsOk = is_trade_both_confirmed(X),
		case IsOk of
			true ->
				trade_execute(X),
				delete_trade(X);
			false -> wait_confirm
		end
	end,
	lookup_cidpair_and_do_transaction(Cid, F).

lookup_cidpair_and_do_transaction(Cid, F) ->
	LF = fun() ->
		case get_trade_cidpair_by_cid(Cid) of
			void -> mnesia:abort(no_trade);
			CidPair -> F(CidPair)
		end
	end,
	case mnesia:transaction(LF) of
		{atomic, Result} -> Result;
		Other -> Other
	end.


%% --transaction実行はここまで。以下はtransactionの内側で呼ばれる関数 --

get_trade_confirmation(CidPair) ->
	reform_trade_confirm_flag(db_trade_confirm_flag(CidPair)).

set_trade_confirm_flag(CidPair, Cid) ->
	[Row] =  qlc:e(qlc:q([X || X <- mnesia:table(trade), X#trade.id == CidPair])),
	case Row of
		{trade, {cid_pair, Cid, _}, _, _} -> mnesia:write(Row#trade{confirm_l = yes});
		{trade, {cid_pair, _, Cid}, _, _} -> mnesia:write(Row#trade{confirm_r = yes});
		_ -> mnesia:abort({ng, "invalid tid"})
	end.

get_trade_cidpair_by_cid(Cid) ->
	case qlc:e(qlc:q([X#u_trade.tid || X<-mnesia:table(u_trade), X#u_trade.cid == Cid])) of
		[] -> mnesia:abort({ng, "no such active character"});
		[Result] -> Result
	end.

db_trade_confirm_flag(CidPair) ->
	case qlc:e(qlc:q([X || X<-mnesia:table(trade), X#trade.id == CidPair])) of
		[] -> mnesia:abort({ng, "no trade"});
		[Result] -> Result
	end.

reform_trade_confirm_flag({trade, {cid_pair, Cid1, Cid2}, Confirm1, Confirm2}) ->
	[{Cid1, Confirm1}, {Cid2, Confirm2}].

reset_trade_confirm_flag(CidPair) ->
	mnesia:write(#trade{id=CidPair, confirm_l = no, confirm_r = no}).

is_trade_both_confirmed(CidPair) ->
	Row =  mnesia:read({trade, CidPair}),
	case Row of
		[{trade, _, CL, CR}] -> CL == yes andalso CR == yes;
		_ -> false
	end.

add_new_trade_row(CidPair) ->
		reset_trade_confirm_flag(CidPair).


% Caution
% Eraser APIs (like delete_trade) are provided to process user operations.
% Do not use them in startup or shutdown.

get_offer_1(Cid) ->
	#inventory{
		cid = Cid,
		money = get_money_by_cid(Cid),
		supplies = get_supplies_by_cid(Cid),
		estate = get_estate_by_cid(Cid)}.

% trade table
%

delete_trade(CidPair) ->
	{cid_pair, Cid1, Cid2} = CidPair,
	delete_trade_row_or_abort(CidPair),
	set_tid_or_abort(Cid1, CidPair, void),
	set_tid_or_abort(Cid2, CidPair, void).

revert_offer(CidPair) ->
	{cid_pair, Cid1, Cid2} = CidPair,
	revert_offer_1(Cid1),
	revert_offer_1(Cid2).

revert_offer_1(Cid) ->
	revert_money_offer(Cid),
	revert_estate_offer(Cid),
	revert_supplies_offer(Cid).

trade_execute(CidPair) ->
	{cid_pair, Cid1, Cid2} = CidPair,
	money_transfer(Cid1, Cid2),
	estate_transfer(Cid1, Cid2),
	supplies_transfer(Cid1, Cid2).

%% トランザクションでくくらずに、各操作を部品として使えるように変更する！！！

% ここから消耗品アイテム（supplies）の操作

get_supplies_by_cid(Cid) ->
	qlc:e(qlc:q([X
		|| X <- mnesia:table(supplies),
			X#supplies.cid == Cid])).

revert_supplies_offer(Cid) ->
	qlc:e(qlc:q([mnesia:write(X#supplies{offer = 0})
		|| X <- mnesia:table(supplies),
			X#supplies.cid == Cid,
			X#supplies.offer > 0])).

%% call example: db:supplies_offer("cid0001", [{item_herb,10}]).
supplies_offer(Cid, ItemIdNumList) ->
	Results = lists:map(fun({ItemId, Num}) ->
		qlc:e(qlc:q([mnesia:write(X#supplies{offer = Num})
			|| X <- mnesia:table(supplies),
				X#supplies.item_id == ItemId,
				X#supplies.cid == Cid,
				X#supplies.amount >= Num]))
		end, ItemIdNumList),
	case lists:member([], Results) of
		true -> mnesia:abort(check_inventory);
		false -> Results
	end.


supplies_add_1(Cid, ItemId, Num) ->
	Result = qlc:e(qlc:q([X || X <- mnesia:table(supplies),
		X#supplies.cid == Cid,
		X#supplies.item_id == ItemId])),

	case Result of
		[] ->
			if
				Num < 0 -> mnesia:abort(check_inventory);
				true ->
					mnesia:write(#supplies{
						id=u:make_new_id(),
						cid=Cid,
						item_id=ItemId,
						amount=Num,
						offer=0})
			end;
		[Sup] ->
			CurrentNum = Sup#supplies.amount,
			if
				CurrentNum + Num < 0 -> mnesia:abort(check_inventory);
				true ->mnesia:write(Sup#supplies{amount= CurrentNum + Num})
			end
	end.


supplies_apply_offer_all(Cid) ->
	qlc:e(qlc:q(
		[mnesia:write(X#supplies{
			amount = X#supplies.amount - X#supplies.offer,
			offer = 0})
			|| X <- mnesia:table(supplies),
				X#supplies.cid == Cid,
				X#supplies.offer > 0])).

supplies_transfer(Cid1, Cid2) ->
	supplies_transfer_1(Cid1, Cid2),
	supplies_transfer_1(Cid2, Cid1).
	
supplies_transfer_1(From, To) ->
	%% 交換対象のサプライ情報抽出
	Sents = qlc:e(qlc:q([X
		|| X <- mnesia:table(supplies),
			X#supplies.cid == From,
			X#supplies.offer > 0])),

	%% 送り側減算処理
	supplies_apply_offer_all(From),

	%% 受け手側加算処理
	lists:map(fun(SentOne) ->
		{supplies, _Id,_Cid,ItemId,_Amount,Offer} = SentOne,
		supplies_add_1(To, ItemId, Offer)
	end, Sents).


% ここからユニークアイテム（estate）の操作
get_estate_by_cid(Cid) ->
	qlc:e(qlc:q([X || X <- mnesia:table(estate), X#estate.cid == Cid])).

revert_estate_offer(Cid) ->
	qlc:e(qlc:q([mnesia:write(X#estate{is_offer = false})
		|| X <- mnesia:table(estate),
			X#estate.cid == Cid])).

estate_offer(Cid, ItemIdList) ->
	Results = lists:map(fun(ItemId) ->
		qlc:e(qlc:q([mnesia:write(X#estate{is_offer = true})
			|| X <- mnesia:table(estate),
				X#estate.item_id == ItemId,
				X#estate.cid == Cid]))
		end, ItemIdList),
	case lists:member([], Results) of
		true -> mnesia:abort(check_inventory);
		false -> Results
	end.

estate_transfer(Cid1, Cid2) ->
	Cid1_Result = estate_transfer_1(Cid1, Cid2),
	Cid2_Result = estate_transfer_1(Cid2, Cid1),
	{ok, {Cid1, Cid1_Result}, {Cid2, Cid2_Result}}.

estate_transfer_1(From, To) ->
	qlc:e(qlc:q([mnesia:write(X#estate{cid = To ,is_offer = false})
		|| X <- mnesia:table(estate),
			X#estate.cid == From,
			X#estate.is_offer == true])).

% ここからお金（money）の操作
get_money_by_cid(Cid) ->
	[C] = mnesia:read(money, Cid),
	C.

revert_money_offer(Cid) ->
	[Money] = mnesia:read({money, Cid}),
	Money1 = Money#money{offer = 0},
	mnesia:write(Money1).

money_offer(Cid, Offer) ->
	[Money] = mnesia:read({money, Cid}),
	Ammount = Money#money.amount,
	if
		Ammount < Offer -> mnesia:abort(check_inventory);
		true ->
			Money1 = Money#money{offer = Offer},
			mnesia:write(Money1)
	end.

money_transfer(Cid1, Cid2) ->
	[CM1] = mnesia:read({money, Cid1}),
	Am1 = CM1#money.amount,
	Offer1 = CM1#money.offer,
	
	[CM2] = mnesia:read({money, Cid2}),
	Am2 = CM2#money.amount,
	Offer2 = CM2#money.offer,

	NewCM1 = CM1#money{amount = Am1 - Offer1 + Offer2, offer = 0},
	NewCM2 = CM2#money{amount = Am2 - Offer2 + Offer1, offer = 0},
	
	mnesia:write(NewCM1),
	mnesia:write(NewCM2).




%% tradeテーブルの操作
delete_trade_row_or_abort(CidPair) ->
	Trades = mnesia:read({trade, CidPair}),
	case Trades of
		[_Trade] -> mnesia:delete({trade, CidPair});
		_ ->mnesia:abort("No such trade.")
	end.

set_tid_or_abort(Key, OrigV, NewV) ->
	[X] = mnesia:read({u_trade, Key}),
	case X#u_trade.tid of
		OrigV -> mnesia:write(X#u_trade{tid = NewV});
		_ -> mnesia:abort(unexpected_value)
	end.



