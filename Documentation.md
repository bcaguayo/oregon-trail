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