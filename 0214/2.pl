notHappy(Dragon) :- isDragon(Dragon), lives(Dragon, Place), isZoo(Place).
happy(Animal) :- isAnimal(Animal), meets(Animal, Person), nice(Person).
nice(Person) :- isPerson(Person), visit(Person, Place), isZoo(Place).
meets(Animal, Person) :- isAnimal(Animal), isPerson(Person), visit(Person, Place), lives(Animal, Place), isZoo(Place).
