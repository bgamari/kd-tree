{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
                
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.KdTree as Kd
import Linear
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Proxy

instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V3 a) where
    arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

basisV3 :: [E V3]
basisV3 = [ex, ey, ez]

basisV2 :: [E V2]
basisV2 = [ex, ey]

axesV3 :: V3 String
axesV3 = V3 "x" "y" "z"

axesV2 :: V2 String
axesV2 = V2 "x" "y"

gen_nearest :: forall f a. (Ord a, Num a, Show a
                           , Metric f, Show (f a), Arbitrary (f a))
             => f String -> [E f] -> Proxy a -> Property
gen_nearest axisNames basis _ = property prop
  where
    prop :: [f a] -> f a -> Property
    prop pts testPt = 
      let tree = Kd.fromVector basis $ V.fromList $ map (,()) pts
          Just (n,_) = Kd.nearest testPt tree
          dist = quadrance (testPt ^-^ n)
      in property $ counterexample (Kd.showKdTree axisNames tree)
         $ all (\p->quadrance (p ^-^ testPt) >= dist) pts

runTests = mapM_ quickCheck
           [ property $ gen_nearest axesV2 basisV2 (Proxy :: Proxy Int)
           , property $ gen_nearest axesV3 basisV3 (Proxy :: Proxy Int)
           ]

main = runTests
