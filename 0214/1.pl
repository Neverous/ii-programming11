likes(Bird, Worm) :-
	isBird(Bird), isWorm(Worm).

likes(Cat, Fish) :-
	isCat(Cat), isFish(Fish).

likes(PersonA, PersonB) :-
	friend(PersonA, PersonB).

likes(PersonA, PersonB) :-
	friend(PersonB, PersonA).

friend(me, my_cat).
eat(my_cat, Food) :-
	likes(my_cat, Food).
