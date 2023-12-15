### The Oregon Trail

We need to do:

Have all modules, types, function declarations, some tests

Modules:
- Text.hs           Store text to display on screen, not testing
                    In the future, read a txt file
                    Miles

- Main.hs           Prints Text
                    Checks GameState

- Resources.hs      Stores Natural numbers
                    Implemented like StateMonad
                    TEST functions like modifyState
                    Miles

- Events.hs         Returns to Main (Event): Outcomes
                    Get user input and changes state
                    Advances Mileage
                    TEST
                    Vince

- GameState.hs      Checks Mileage
                    Ends the Game
                    TEST

- check if game end condition has been achieved

- Regular Options:
1. Keep going
2. talk to locals
3. shop
4. hunt
...

- Random Events: pick three or four

* Ask TAs

Testing:
1. Main.hs: IO Test Input
2. GameState.hs: Expand HUnit
3. Resources.hs: Use QC
    Consuming food should decrease the amount of food available.
    Buying clothes should decrease the amount of money available.
    Trading resources should result in the correct exchange of goods.
4. Options.hs: Use QC
    All valid commands should be parsed correctly.
    Invalid commands should be rejected.
    Parsing commands should not produce errors.
5. Locations.hs: Use QC
    The displayed options should be valid for the current location.
    The displayed events should be possible for the current state of the game.
    The displayed information should be consistent with the game state.
6. Events.hs: ?
7. Test.hs: Hard coded values, No testing required. 
8. State.hs: [Import]
9. StateMonad.hs: [Import]

WIP:
- Get State and StateMonad from Solutions or delete
- ask about substraction
- make new map
- make options
- get testing

Itinerary Dec 1 2:30pm
1. Go over code
3. Issues
    - randomizing events
    - IO Tests
    - structural issues

Demo Day Functionality
1. Print Game State on Each Loop
2. Shop for Food 
3. Travel Update
4. Game End Slow
5. Game End Good

Demo Day OH:
- Am I using the State Monad Correctly      - Refactor at Cassias OH
- why does HLint yell at me                 - not using stack build
- How do I make IO Tests                    - Concurrency
- How do I make Event QC
- Change "Insufficient Food" to be a FrontEnd Exception
- hard code text yay or nay

Test Quickcheck:
- IO Tests
- Events

Test HUnit:
- GameState: endGood, endIll, endSlow
- Resources

- ----| stack build and reopen to reboot hlint

### TODO Split:
- Implement Monadic Transformers
- Implement Concurrency: inputb, output
    substractResources :: Resources s -> ResourceType -> Nat -> Either String (Resources s)
- Transformers ExceptionT:
        type ExceptT :: Type -> (Type -> Type) -> Type -> Type
        newtype ExceptT e m a = MkExc {runExceptT :: m (Either e a)}
    layer String on top of State | ExceptT String (GameStateM)

Vince:
- IO Tests
- Change GS tests to GST

Miles:
- Change "Insufficient Food" to be a FrontEnd Print instead of an error
- Events Tests


WIP:
- Shopping Functions (M)
- shopAction (V)
- Events - Locations - Options Loop
- Testing for these 

- New Resources: Bullets, Oxen, Medicine, Wheels.
- QC for Resources
- Hunting: substract bullets, pass one day, add Random Range of food depending on marksmanship.

- Banker: Start with 1000, worst at hunting
- Carpenter: Start with 700, average at hunting
- Farmer: Start with 400, best at hunting

professions:
- Main
- GameState 

Miles:
- Event-Location-Options Loop
- Event-Location-Options Testing
- Shopping Functions
- Professions on Main
- Trace Tests

Vince:
- Professions on GameState
- New Resources
- QC for Resources
- Hunting
- shopActionM


{-
WIP, move Dates code to Events.hs or smth
min Date is March 29 (1)
max Date is December 20 (266)

intermediate Dates are every 14 days
April 12 (15)
April 26 (29)
May 10 (43)
May 24 (57)
June 7 (71)
June 21 (85)
July 5 (99)
July 19 (113)
August 2 (127)
August 16 (141)
August 30 (155)
September 13 (169)
September 27 (183)
October 11 (197)

October 25 (211) 
November 8 (225)
November 22 (239)
December 6 (253)
December 20 (266)

without events there are 20 intermediate dates
which means 20 updates/steps

Pace Fast should reach 2000 miles in 200 days
so 14 steps, that's 2000 / 14 = 142.85714285714286 miles per step
let's do 145 miles for fast

Pace Slow shouldn't reach Oregon in time
so 20 * pace < 2000,
pace < 100 miles per step
lets do 95 miles for slow

Multiples of 145 until 2040
145, 290, 435, 580, 725, 870, 1015, 1160, 1305, 1450, 1595, 1740, 1885, 2030

Multiples of 95 until 2040
95, 190, 285, 380, 475, 570, 665, 760, 855, 950, 1045, 1140, 1235, 1330, 1425, 1520, 1615, 1710, 1805, 1900, 1995

-}

{-
Hits:
Independence, Missouri  0 miles
Missouri River          10 miles
Fort Kearney            325 miles
Fort Laramie            600 miles
Fort Bridger            975 miles
Fort Boise              1500 miles
The Dalles              1780 miles
Oregon City, Oregon     2000 miles
-}

{-
The exact distance traveled on the Oregon Trail varied depending on the specific 
route taken and the number of detours or side trips made. 
However, the average distance from Independence, Missouri, to Oregon City, 
Oregon, was approximately 2,170 miles (3,490 kilometers). 
This distance could be as short as 2,000 miles (3,200 kilometers) or as long as 
2,500 miles (4,000 kilometers), depending on the route.

Missouri River to Fort Kearney: 325 miles (523 kilometers)
Fort Kearney to Fort Laramie: 250 miles (402 kilometers)
Fort Laramie to Fort Bridger: 400 miles (644 kilometers)
Fort Bridger to Fort Boise: 580 miles (933 kilometers)
Fort Boise to The Dalles: 315 miles (507 kilometers)
The Dalles to Oregon City: 300 miles (483 kilometers)

-}

{-
March starts 29th (1) and ends 31st (3)
April starts 1st (4) and ends 30th (34)
May starts 1st (35) and ends 31st (65)
June starts 1st (66) and ends 30th (96)
July starts 1st (97) and ends 31st (127)
August starts 1st (128) and ends 31st (158)
September ^starts 1st (159) and ends 30th (189)
October star^ts 1st (190) and ends 31st (220)
November starts 1st (221) and ends 30th (251)
December starts 1st (252) and ends 20th (266)
-}