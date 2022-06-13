location(T1,R,C) :-
  T0  is T1 - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  (
    ((action(T0,eat);action(T0,clockWise);action(T0,counterClockWise)), location(T0,R,C));
    ((action(T0,attack);action(T0,forward)), bump(T1), location(T0,R,C));
    ((action(T0,attack);action(T0,forward)), dir(T0,north), not(bump(T1)), location(T0,RS,C));
    ((action(T0,attack);action(T0,forward)), dir(T0,south), not(bump(T1)), location(T0,RN,C));
    ((action(T0,attack);action(T0,forward)), dir(T0,west),  not(bump(T1)), location(T0,R,CE));
    ((action(T0,attack);action(T0,forward)), dir(T0,east),  not(bump(T1)), location(T0,R,CW))
  ).

dir(T1,north) :-
  T0 is T1 - 1,
		(
				((action(T0,eat);action(T0,attack);action(T0,forward)), dir(T0,north) );
				(action(T0,clockWise)       , dir(T0,west));
				(action(T0,counterClockWise), dir(T0,east))
		).

dir(T1,east) :-
  T0 is T1 - 1,
		(
				((action(T0,eat);action(T0,attack);action(T0,forward)), dir(T0,east));
				(action(T0,clockWise)       , dir(T0,north));
				(action(T0,counterClockWise), dir(T0,south))
		).

dir(T1,south) :-
  T0 is T1 - 1,
		(
				((action(T0,eat);action(T0,attack);action(T0,forward)), dir(T0,south));
				(action(T0,clockWise)       , dir(T0,east));
				(action(T0,counterClockWise), dir(T0,west))
		).

dir(T1,west) :-
  T0 is T1 - 1,
		(
				((action(T0,eat);action(T0,attack);action(T0,forward)), dir(T0,west) );
				(action(T0,clockWise)       , dir(T0,south));
				(action(T0,counterClockWise), dir(T0,north))
		).


/* Wall variables related */

isEat(T,R,C) :-
  T0 is T - 1,
  (
    (
      location(T,R,C),
      action(T,eat)
    );
    (
      T > 1,
      isEat(T0,R,C)
    )
  ).
isEat(R,C):-
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  isEat(TT,R,C).

isWall(T1,R,C) :- 
	T0  is T1 - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  (
    ((action(T0,attack);action(T0,forward)), dir(T0,north), bump(T1), location(T0,RS,C));
    ((action(T0,attack);action(T0,forward)), dir(T0,south), bump(T1), location(T0,RN,C));
    ((action(T0,attack);action(T0,forward)), dir(T0,west),  bump(T1), location(T0,R,CE));
    ((action(T0,attack);action(T0,forward)), dir(T0,east),  bump(T1), location(T0,R,CW));
    (T0 > 0, isWall(T0,R,C))
  ).
isWall(R,C):-
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  (
    R < 1;
    C < 1;
    isWall(TT,R,C)
  ).

isClear(T,R,C) :-
  hasNotWumpus(T,R,C),
  hasNotPit(T,R,C),
  not(isWall(R,C)).

isVisitedHelper(T,R,C) :-
  T0 is T1 - 1,
  location(T,R,C);
  (
    T > 1,
    isVisitedHelper(T0,R,C)
  ).
isVisited(R,C) :-
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  isVisitedHelper(TT,R,C).

isDangerousOrWall(R,C) :-
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  (
    isWall(R,C);
    hasWumpus(TT,R,C);
    hasPit(TT,R,C)
  ).

hasNotWumpusHelper(T,R,C) :-
  T0  is T - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  (
    isWall(R,C);
    location(T,R,C);
    (not(wumpusSmell(T)), location(T,RS,C));
    (not(wumpusSmell(T)), location(T,RN,C));
    (not(wumpusSmell(T)), location(T,R,CE));
    (not(wumpusSmell(T)), location(T,R,CW));
    (T0 > 0, hasNotWumpusHelper(T0,R,C))
  ).
hasNotWumpus(T0,R,C) :-
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  hasNotWumpusHelper(TT,R,C).

hasNotPitHelper(T,R,C) :- 
  T0  is T - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  (
    isWall(R,C);
    location(T,R,C);
    (not(pitBreeze(T)), location(T,RS,C));
    (not(pitBreeze(T)), location(T,RN,C));
    (not(pitBreeze(T)), location(T,R,CE));
    (not(pitBreeze(T)), location(T,R,CW));
    (T0 > 0, hasNotPitHelper(T0,R,C))
  ).

hasNotPit(T0,R,C) :- 
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  hasNotPitHelper(TT,R,C).

hasPitHelper(T,R,C) :- 
  T0  is T - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  RNN is RN - 1,
  RSS is RS + 1,
  CWW is CW - 1,
  CEE is CE + 1,
  (
    ((pitBreeze(T), location(T,RS,C)), (hasNotPit(T,RSS,C), hasNotPit(T,RS,CE), hasNotPit(T,RS,CW)));
    ((pitBreeze(T), location(T,RN,C)), (hasNotPit(T,RNN,C), hasNotPit(T,RN,CE), hasNotPit(T,RN,CW)));
    ((pitBreeze(T), location(T,R,CE)), (hasNotPit(T,RN,CE), hasNotPit(T,RS,CE), hasNotPit(T,R,CEE)));
    ((pitBreeze(T), location(T,R,CW)), (hasNotPit(T,RN,CW), hasNotPit(T,R,CWW), hasNotPit(T,RS,CW)));
    (T0 > 0, hasPitHelper(T0,R,C))
  ).


hasPit(T0,R,C) :- 
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  hasPitHelper(TT,R,C).

hasWumpusHelper(T,R,C) :- 
  T0  is T - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  RNN is RN - 1,
  RSS is RS + 1,
  CWW is CW - 1,
  CEE is CE + 1,
  (
    ((wumpusSmell(T), location(T,RS,C)), (hasNotWumpus(T,RSS,C), hasNotWumpus(T,RS,CE), hasNotWumpus(T,RS,CW)));
    ((wumpusSmell(T), location(T,RN,C)), (hasNotWumpus(T,RNN,C), hasNotWumpus(T,RN,CE), hasNotWumpus(T,RN,CW)));
    ((wumpusSmell(T), location(T,R,CE)), (hasNotWumpus(T,RN,CE), hasNotWumpus(T,RS,CE), hasNotWumpus(T,R,CEE)));
    ((wumpusSmell(T), location(T,R,CW)), (hasNotWumpus(T,RN,CW), hasNotWumpus(T,R,CWW), hasNotWumpus(T,RS,CW)));
    (T0 > 0, hasWumpusHelper(T0,R,C))
  ).


hasWumpus(T0,R,C) :- 
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  hasWumpusHelper(TT,R,C).

hasDeadWumpusHelper(T,R,C) :-
  T0  is T - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  hasWumpus(T,R,C),
  (
    (action(T,attack), dir(T,north), location(T,RS,C));
    (action(T,attack), dir(T,south), location(T,RN,C));
    (action(T,attack), dir(T,west),  location(T,R,CE));
    (action(T,attack), dir(T,east),  location(T,R,CW));
    (T0 > 0, hasDeadWumpusHelper(T0,R,C))
  ).

hasDeadWumpus(T0,R,C) :-
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  hasDeadWumpusHelper(TT,R,C).

hasFoodHelper(T,R,C) :- 
  T0  is T - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  RNN is RN - 1,
  RSS is RS + 1,
  CWW is CW - 1,
  CEE is CE + 1,
  not(isEat(R,C)),
  (
    ((foodSmell(T), location(T,RS,C)), (hasNotFood(T,RSS,C), hasNotFood(T,RS,CE), hasNotFood(T,RS,CW)));
    ((foodSmell(T), location(T,RN,C)), (hasNotFood(T,RNN,C), hasNotFood(T,RN,CE), hasNotFood(T,RN,CW)));
    ((foodSmell(T), location(T,R,CE)), (hasNotFood(T,RN,CE), hasNotFood(T,RS,CE), hasNotFood(T,R,CEE)));
    ((foodSmell(T), location(T,R,CW)), (hasNotFood(T,RN,CW), hasNotFood(T,R,CWW), hasNotFood(T,RS,CW)));
    (T0 > 0, hasFoodHelper(T0,R,C))
  ).

hasFood(T0,R,C) :- 
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  hasFoodHelper(TT,R,C).

hasNotFoodHelper(T,R,C) :-
  T0  is T - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  (
    isWall(R,C);
    (not(foodSmell(T)), (location(T,RS,C)));
    (not(foodSmell(T)), (location(T,RN,C)));
    (not(foodSmell(T)), (location(T,R,CE)));
    (not(foodSmell(T)), (location(T,R,CW)));
    (T0 > 0, hasNotFoodHelper(T0,R,C))
  ).

hasNotFood(T0,R,C) :-
  findall(T, action(T, X), L),
  last(T, L),
  TT is T+1,
  hasNotFoodHelper(T0,R,C).

last(X,[_|Z]) :- last(X,Z).
last(X,[X]).

/* Define */
bump(-1).
pitBreeze(-1).
wumpusSmell(-1).
foodSmell(-1).
action(-1, -1).

/* Experience */
location(1,1,1).
dir(1,east).
action(1,forward).
action(2,counterClockWise).
action(3,counterClockWise).
action(4,forward).
action(5,counterClockWise).
action(6,forward).
action(7,counterClockWise).
action(8,forward).
action(9,forward).
action(10,counterClockWise).
action(11,attack).
wumpusSmell(2).
wumpusSmell(3).
wumpusSmell(4).
pitBreeze(7).
pitBreeze(8).
foodSmell(10).
wumpusSmell(10).
foodSmell(11).
wumpusSmell(11).