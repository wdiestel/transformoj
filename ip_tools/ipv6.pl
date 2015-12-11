:- use_module(library('http/dcg_basics')).


ipv6_value(IPAtom,ValueList) :-
  atom(IPAtom),
  atom_codes(IPAtom,IPCodes),
  phrase(ipv6(ValueList),IPCodes).

% FIXME: separate list from atom predicate...
ipv6_value(IPCodes,ValueList) :-
  is_list(IPCodes),
  phrase(ipv6(ValueList),IPCodes).

ipv6_value(IPCodes,ValueList) :-
  var(IPCodes),
  is_list(IPCodes),
  phrase(ipv6(ValueList),IPCodes).


ipv6_long(ValueList,IPAtom) :-
  var(IPAtom),
  format(atom(IPAtom),'~|~`0t~16r~4+:~|~`0t~16r~4+:~|~`0t~16r~4+:~|~`0t~16r~4+:~|~`0t~16r~4+:~|~`0t~16r~4+:~|~`0t~16r~4+:~|~`0t~16r~4+',ValueList).


ipv6_short(ValueList,IPAtom) :-
  var(IPAtom), length(ValueList,8),
  max_zero_sublist(ValueList,Start,Len) *->
  (
     length(Prefix,Start),
     append(Prefix,_,ValueList),
     SLen is 8 - Len - Start,
     length(Suffix,SLen),
     append(_,Suffix,ValueList),
     %format(atom(IPAtom),'~w :: ~w',[Prefix,Suffix])
     hex_atom(Prefix,PrefixHex), 
     hex_atom(Suffix,SuffixHex), 
     atomic_list_concat([PrefixHex,SuffixHex],'::',IPAtom)
 %...
  ) ;
  % no sublist with zeroes found...
  format(atom(IPAtom),'~16r:~16r:~16r:~16r:~16r:~16r:~16r:~16r',ValueList).

hex_atom(ValueList,HexAtom) :-
  hex_list(ValueList,HexList),
  atomic_list_concat(HexList,':',HexAtom).

hex_list([],[]).
hex_list([Val|ValList],[Hex|HexList]) :-
  format(atom(Hex),'~16r',Val),
  hex_list(ValList,HexList).
  

ipv6_localhost([0,0,0,0,0,0,0,1]).


max_zero_sublist(ValueList,Start,Len) :-
  phrase(z_interval(0,'',Intervals),ValueList),
  find_max(Intervals,(Start,Len)),
  Len > 0.


%%%%%%%%%%%%%%%%% finding maximum interval of zeros

% test:  zero_intervals([0,1,0,0,1,1,0,0,0,0],0,Is,'').
% Is = [ (0, 1), (2, 2), (6, 4)] ;

zero_intervals([],_,[(P,L)],(P,L)).
zero_intervals([],_,[],'').

zero_intervals([0|RList],Pos,Intervals,(P,L)) :-
  % there was L zeroes before...
  Pos =:= P+L,
  % go deeper into list
  Pos1 is Pos+1,
  L1 is L+1,
  zero_intervals(RList,Pos1,Intervals,(P,L1)).

zero_intervals([0|RList],Pos,[(P,L)|Intervals],(P,L)) :-
  % there was no zero before...
  Pos > P+L,
  Pos1 is Pos+1,
  zero_intervals(RList,Pos1,Intervals,(Pos,1)).

zero_intervals([X|RList],Pos,[(P,L)|Intervals],(P,L)) :-
  % a series of zeros finishes
  X \= 0,
  Pos1 is Pos+1,
  zero_intervals(RList,Pos1,Intervals,'').

zero_intervals([0|RList],Pos,Intervals,'') :-
  % a starting zero found in list
  Pos1 is Pos+1,
  zero_intervals(RList,Pos1,Intervals,(Pos,1)).

zero_intervals([X|RList],Pos,Intervals,'') :-
  % a starting nonzero found in list
  X \= 0,
  Pos1 is Pos+1,
  zero_intervals(RList,Pos1,Intervals,'').



find_max([],(-1,-1)).

find_max([(P,L)|Intervals],(P,L)) :-
  find_max(Intervals,(_,L1)),
  L1 =< L.

find_max([(_,L)|Intervals],(P1,L1)) :-
  find_max(Intervals,(P1,L1)),
  L1 >= L.


%%%%%%%%%%%%%%%%% DCG for finding maximum interval of zeros

% test:  phrase(z_intervals(0,'',Is),[0,1,0,0,1,1,0,0,0,0]).
% Is = [ (0, 1), (2, 2), (6, 4)] ;

z_interval(_,(P,L),[(P,L)]) --> [].
z_interval(_,'',[]) --> [].

z_interval(Pos,(P,L),Intervals) --> [0], 
  { Pos =:= P + L, Pos1 is Pos + 1, L1 is L + 1 },
  z_interval(Pos1,(P,L1),Intervals).

z_interval(Pos,(P,L),[(P,L)|Intervals]) --> [0],
  { Pos > P + L, Pos1 is Pos + 1 },
  z_interval(Pos1,(Pos,1),Intervals).

z_interval(Pos,(P,L),[(P,L)|Intervals]) --> [X],
  { X \= 0, Pos1 is Pos + 1 },
  z_interval(Pos1,'',Intervals).

z_interval(Pos,'',Intervals) --> [0],
  { Pos1 is Pos+1 },  
  z_interval(Pos1,(Pos,1),Intervals).

z_interval(Pos,'',Intervals) --> [X],
  { X \= 0, Pos1 is Pos + 1 },
  z_interval(Pos1,'',Intervals).


%%%%%%%%% DCG for parsing IPv6 

%test: phrase(ipv6(I),"2001:db8::1428:57ab").
ipv6(IPv6) --> ipv6_canonical(IPv6) ; ipv6_prefix_suffix(IPv6).
ipv6_canonical(IPv6) --> ipv6_part(IPv6,8).
ipv6_prefix_suffix(IPv6) --> ipv6_part(Prefix,PL), "::", ipv6_part(Suffix,SL), 
   { 
     zero_list(ZList,8-PL-SL),
     append([Prefix,ZList,Suffix],IPv6) 
   }.

ipv6_part([],0) --> [].
ipv6_part([I],1) --> xinteger(I).
ipv6_part(I,N) --> xinteger(I1), ":", ipv6_part(I2,N1), { append([I1],I2,I), N is N1+1 }.

zero_list([],0).
zero_list([0|ZList],Len) :-
  Len > 0, Len1 is Len - 1,
  zero_list(ZList,Len1).

  
