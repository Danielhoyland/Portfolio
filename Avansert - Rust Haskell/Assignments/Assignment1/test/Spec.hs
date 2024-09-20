import Test.DocTest(doctest)
import Test.Hspec(Spec, descibe, shouldBe, it, hspec)

import Lib(addGroups)
main :: IO ()
main = do
    doctest["-isrc", "app/Main.hs"]
    hspec testFunc

testFunc :: Spec
testFunc = 
    descibe "Testing Lib module" $ do
        descibe "addToGroups" $ do
            it "works for world (0,0) White" $ do
                addGroups world (0,0) White `shouldBe` ([(White,[(1,([(0,0)][(1,0),(0,1)]))]),(Black,[])],True)
        descibe "flipColor" $ do
            it "works for White" $ do
                flipColor White `shouldBe` Black
            it "works for Black" $ do
                flipColor Black `shouldBe` White
        descibe "checkAvailable" $ do
            it "works for 0 0 [[Nothing,Nothing][Nothing,Nothing]]" $ do
                checkAvailable 0 0 [[Nothing,Nothing][Nothing,Nothing]] `shouldBe` True
            it "don't work for (-1) 0 [[Nothing,Nothing][Nothing,Nothing]]" $ do
                checkAvailable (-1) 0 [[Nothing,Nothing][Nothing,Nothing]] `shouldBe` False
        descibe "libPos" $ do
            it "works for (1,1)" $ do
                libPos (1,1) `shouldBe` [(0,1),(2,1),(1,0),(1,2)]
        descibe "libPos" $ do
            it "works for (1,1)" $ do
                libPos (1,1) `shouldBe` [(0,1),(2,1),(1,0),(1,2)]