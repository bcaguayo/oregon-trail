-- implement QC
module ResourcesT where

import Resources
import Test.QuickCheck ()

-- Custom Arbitrary implementation
instance Arbitrary ResourceType where
  arbitrary = elements [Food, Clothes, Money, Bullets, Oxen, Medicine, Wheels]

  -- No need to shrink ResourceType because it is an enumerated type
  shrink _ = []

instance Arbitrary (Resources s) where
  arbitrary = Resources <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  -- Shrink Resources instance
  shrink r =
    [ r {food = f, clothes = c, money = m, bullets = b, oxen = o, medicine = med, wheels = w}
      | f <- shrink (food r),
        c <- shrink (clothes r),
        m <- shrink (money r),
        b <- shrink (bullets r),
        o <- shrink (oxen r),
        med <- shrink (medicine r),
        w <- shrink (wheels r)
    ]

-- Test that zeroResources returns zero for all resource types
prop_zeroResources :: Bool
prop_zeroResources =
  all (\r -> getResourceAmount zeroResources r == 0) [Food, Clothes, Money, Bullets, Oxen, Medicine, Wheels]

-- Test the initial setup of initialResources
prop_initialResources :: Bool
prop_initialResources =
  money initialResources == 700 && all (\r -> getResourceAmount initialResources r == 0) [Food, Clothes, Bullets, Oxen, Medicine, Wheels]

-- Test the addition of resources
prop_addResources :: ResourceType -> NonNegative Integer -> Bool
prop_addResources rtype (NonNegative n) =
  let natN = fromInteger n
   in getResourceAmount (addResources' zeroResources rtype natN) rtype == natN

-- Test the subtraction of resources and error handling
prop_subtractMoney :: NonNegative Integer -> NonNegative Integer -> Property
prop_subtractMoney (NonNegative m) (NonNegative n) =
  m >= n ==>
    let natM = fromInteger m
        natN = fromInteger n
     in money (substractMoney (addMoney zeroResources natM) natN) == natM - natN

-- Test the conversion from Nat to ResourceType
prop_natToRes :: NonNegative Integer -> Bool
prop_natToRes (NonNegative n) =
  let natN = fromInteger n
   in case natToRes natN of
        Food -> natN == 0
        Clothes -> natN == 1
        Bullets -> natN == 2
        Oxen -> natN == 3
        Medicine -> natN == 4
        Wheels -> natN == 5
        Money -> natN > 5

runTests :: IO ()
runTests = do
  quickCheck prop_zeroResources
  quickCheck prop_initialResources
  quickCheck prop_addResources
  quickCheck prop_subtractMoney
  quickCheck prop_natToRes