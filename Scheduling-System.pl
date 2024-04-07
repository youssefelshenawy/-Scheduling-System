ta_slot_assignment([ta(Name,L)|T],[ta(Name,S)|T],Name) :-
		L > 0,
		S is L - 1.
ta_slot_assignment([ta(Name,0)|T],[ta(Name,0)|T],Name).

ta_slot_assignment([ta(N,L)|T],[ta(N,L)|T1],Name):-
		Name \= N,
		ta_slot_assignment(T,T1,Name).
		

combination(0,_,[]).
combination(N,[H|T],[H|T1]):-
	N > 0,
	N1 is N - 1,
	combination(N1,T,T1).
combination(N,[_|T],L):-
	N >0 ,
	combination(N,T,L).

%get_zero(TAs list, list which has TAs' names with zero load).
get_zero([],[]).	
get_zero([H|T],[N|T1]):-
	H = ta(N,0),
	get_zero(T,T1).
get_zero([H|T],L2):-
	H = ta(_,L),
	L > 0,
	get_zero(T,L2).

%remove_zero(TAs' names list, list of TAs names with zero load, TAs names with load not zero).	
remove_zero(L,[],L).	
remove_zero([H|T],[H|T1],L2):-
	remove_zero(T,T1,L2).
remove_zero([H|T],[H1|T1],[H|L2]):-
	H \= H1,
	remove_zero(T,[H1|T1],L2).
	

similar([],_).
similar([H|T],L):-
		member(H,L),
		similar(T,L).

included_in(Assignment,[H|_]):-
		permutation(Assignment,H).
included_in(Assignment,[H|T]):-
		\+permutation(Assignment,H),
		included_in(Assignment,T).

names([],[]).
names([H|T],[H1|T1]):-
		H = ta(N,_),
		H1 = N,
		names(T,T1).

slot_assignment(0,TAs,TAs,[]).
%NT = TAs names , ZT = TAs with zero load, NZT = no zero load TAs
slot_assignment(LabsNum,TAs,RemTAs,Assignment):-
		names(TAs,NT),
		get_zero(TAs,ZT),
		remove_zero(NT,ZT,NZT),
		setof(L,combination(LabsNum,NZT,L),List),
		(member(Assignment,List);included_in(Assignment,List)),
		LabsNum>0,
		slot_assignment1(LabsNum,TAs,RemTAs,Assignment).

slot_assignment1(0,TAs,TAs,[]).
slot_assignment1(LabsNum,[ta(N,0)|T],[ta(N,0)|TR],Assignment):-
		\+member(N,Assignment),
		delete(N,Assignment,A),
		slot_assignment1(LabsNum,T,TR,A).
slot_assignment1(LabsNum,TAs,RemTAs,[N|Ta]):-
		LabsNum > 0,
		RemLabs is LabsNum - 1,
		ta_slot_assignment(TAs,[HRem|TRem],N),
		slot_assignment1(RemLabs,[HRem|TRem],RemTAs,Ta).




		
day_schedule([],TAs,TAs,[]).
day_schedule([H|T],TAs,RemTAs,[Ha|Ta]):-
		H > 0,
		slot_assignment(H,TAs,RemTAs1,HAssignment),
		setof(L,permutation(HAssignment,L),List),
		member(Ha,List),
		day_schedule(T,RemTAs1,RemTAs,Ta).
day_schedule([0|T],TAs,RemTAs,[[]|A]):-
		day_schedule(T,TAs,RemTAs,A).

occurr_in_list([],_,0).
occurr_in_list([H|T],H,S):-
		occurr_in_list(T,H,S1),
		S is S1 + 1.
occurr_in_list([H|T],O,S):-
		H \= O,
		occurr_in_list(T,O,S).
max_slots_per_day([],_).
max_slots_per_day(DaySched,Max):-
 		flatten(DaySched,List),
		max_slots_per_day1(List,Max).

max_slots_per_day1([],_).
max_slots_per_day1([H|T],Max):-
		occurr_in_list([H|T],H,O),
		O =< Max,
		max_slots_per_day1(T,Max).		

	
week_schedule([],_,_,[]).		
week_schedule([H|T],TAs,DayMax,[Hw|Tw]):-
		day_schedule(H,TAs,RemTAs,Hw),
		max_slots_per_day(Hw,DayMax),
		week_schedule(T,RemTAs,DayMax,Tw).
		



		
