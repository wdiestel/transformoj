help :-
  format('ip_numeric/2, mask_min_max/3, ip_mask/2~n',[]).


% conversion between IP dot representation and numeric value
ip_numeric(IP,Numeric) :-
  atom(IP),
  atomic_list_concat([B1,B2,B3,B4],'.',IP),
  atom_number(B1,N1), atom_number(B2,N2), atom_number(B3,N3), atom_number(B4,N4), 
  Numeric is N1*256*256*256 + N2*256*256 + N3*256 + N4.
  
% conversion between IP dot representation and numeric value
ip_numeric(IP,Numeric) :-
  \+ atom(IP),
  N4 is mod(Numeric,256),
  R3 is div(Numeric,256),
  N3 is mod(R3,256),
  R2 is div(R3,256),
  N2 is mod(R2,256),
  R1 is div(R2,256),
  N1 is mod(R1,256),
  atomic_list_concat([N1,N2,N3,N4],'.',IP).

% calculates Min and Max as numeric IP for Network mask 
mask_min_max(Mask,Min,Max) :-
  atomic_list_concat([MinIP,Bits],'/',Mask),
  ip_numeric(MinIP,Min), atom_number(Bits,B),
  Max is Min + 2 ** (32-B) - 1.
  
% ip belongs to network mask 
ip_mask(IP,Mask) :-
  mask_min_max(Mask,Min,Max),  
  between(Min,Max,Num),
  ip_numeric(IP,Num).
  
bit_mask(Bits,Mask) :-
   Mask is 1<<32 - 1<<(32-Bits).
  
ip_nw(IP,SubnetBits,NW) :-
  ip_numeric(IP,Num),
  bit_mask(SubnetBits,Mask),
  NWNum is Mask /\ Num,
  ip_numeric(NW,NWNum).
  

networks_to_pairs([],[]). 
networks_to_pairs([N/B|Rest],[N-B|Pairs]) :- networks_to_pairs(Rest,Pairs),!. 
networks_to_pairs([Nw|Rest],[N-B|Pairs]) :- 
  atomic_list_concat([N,Ba],'/',Nw), atom_number(Ba,B),
  networks_to_pairs(Rest,Pairs). 
  
mask_ip(N,Mask,IP) :-  
      ip_numeric(IP,I),
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
  ip_numeric(NW,N),!.
  
  
route_includes(Route/RBits,NW/NBits) :-
  ip_numeric(Route,R),
  ip_numeric(NW,N),
  RBits =< NBits,
  bit_mask(RBits,Mask),
  N /\ Mask =:= R.
  
routes_excludes([],Nw,Nw).
  
routes_excludes([Route|Routes],Networks,NWRest) :-
  exclude(route_includes(Route),Networks,Rest),
  routes_excludes(Routes,Rest,NWRest).
  
  
