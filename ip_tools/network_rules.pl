help :-
  format('ip_int/2, mask_min_max/3, ip_mask/2~n',[]).


% conversion between IP dot representation and numeric value
ip_int(IP,Numeric) :-
  atom(IP),
  atomic_list_concat([B1,B2,B3,B4],'.',IP),
  atom_number(B1,N1), atom_number(B2,N2), atom_number(B3,N3), atom_number(B4,N4), 
  Numeric is N1<<24 + N2<<16 + N3<<8 + N4.
  
% conversion between IP dot representation and numeric value
ip_int(IP,Numeric) :-
  \+ atom(IP),
  Mask = 255,
  N4 is (Numeric /\ Mask),
  N3 is (Numeric /\ (Mask<<8)) >>8,
  N2 is (Numeric /\ (Mask<<16)) >>16,
  N1 is (Numeric /\ (Mask<<24)) >>24,
  atomic_list_concat([N1,N2,N3,N4],'.',IP).

/***
% calculates Min and Max as numeric IP for Network mask 
mask_min_max(Mask,Min,Max) :-
  atomic_list_concat([MinIP,Bits],'/',Mask),
  ip_int(MinIP,Min), atom_number(Bits,B),
  Max is Min + (1 << (32-B)) - 1.
  
% ip belongs to network mask 
ip_mask(IP,Mask) :-
  mask_min_max(Mask,Min,Max),  
  between(Min,Max,Num),
  ip_int(IP,Num).
  ***/

bit_mask(Bits,Mask) :-
   Mask is 1<<32 - 1<<(32-Bits).

ip_nw(IP,SubnetBits,NW) :-
  atom(IP),
  ip_int(IP,Num),
  Mask is 1<<32 - 1<<(32-SubnetBits),
  NWNum is Mask /\ Num,
  ip_int(NW,NWNum).

ip_nw(IP,SubnetBits,NW) :-
  atom(NW),
  ip_int(NW,Min),
  Max is Min + (1 << (32-SubnetBits)) - 1,
  between(Min,Max,Num),
  ip_int(IP,Num).
  

networks_to_pairs([],[]). 
networks_to_pairs([N/B|Rest],[N-B|Pairs]) :- networks_to_pairs(Rest,Pairs),!. 
networks_to_pairs([Nw|Rest],[N-B|Pairs]) :- 
  atomic_list_concat([N,Ba],'/',Nw), atom_number(Ba,B),
  networks_to_pairs(Rest,Pairs). 
  
mask_ip(N,Mask,IP) :-  
      ip_int(IP,I),
      N is I /\ Mask.
  
summarize(Networks,NW/Bits) :-
  networks_to_pairs(Networks,Pairs),
  pairs_values(Pairs,BitLens),
  pairs_keys(Pairs,IPs),  
  min_list(BitLens,MinBits),
  between(0,MinBits,B_),
  Bits is MinBits-B_,
 % format('b ~w~n',Bits),
  bit_mask(Bits,Mask),
  maplist(mask_ip(N,Mask),IPs),
  ip_int(NW,N),!.
  
  
route_includes(Route/RBits,NW/NBits) :-
  ip_int(Route,R),
  ip_int(NW,N),
  RBits =< NBits,
  bit_mask(RBits,Mask),
  N /\ Mask =:= R.
  
routes_excludes([],Nw,Nw).
  
routes_excludes([Route|Routes],Networks,NWRest) :-
  exclude(route_includes(Route),Networks,Rest),
  routes_excludes(Routes,Rest,NWRest).
  
  
