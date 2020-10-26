:- use_module(library(chr)).
:- chr_constraint water/0, salt/0, salt_water/0, stir/0, time/0, noise/0, backtrack/1, found/1.
salt, salt <=> salt.
water,water <=> water.
salt, water, stir <=> salt_water, stir.
stir ==> noise.
stir <=> true.
salt_water, time <=> salt.
water, time <=> true.
time <=> true.

backtrack(X) ==> member(X, [a,b,c]), found(X).

p_coin :- heads.
p_coin :- tails.
heads.
tails.
