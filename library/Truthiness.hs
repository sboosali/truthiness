{-# LANGUAGE LambdaCase, DoAndIfThenElse, GeneralizedNewtypeDeriving #-} 
module Truthiness where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid (Any(..), All(..))
import System.Exit (ExitCode(..)) 
import Numeric.Natural (Natural)
import Data.Complex (Complex(..))
import Data.Ratio (Ratio)
import Control.Applicative (Alternative(..), Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Foldable (Foldable(..))


{-| Pythonic truthiness/falsines. see <https://docs.python.org/2.4/lib/truth.html>.

keep around information (e.g. use @Maybe@, a custom flag type, etc.)
to avoid <https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/ Boolean Blindness>, 
then drop that information with 'Boolean', when necessary for convenience.  


e.g. 'when'


when 'Foldable', instances should satisfy the following law: 

@
'falsy' = 'null' . 'toList'
@


when a 'Monoid', instances should satisfy the following law: 

@
'falsy' = ('mempty' ==) 
@


when a 'Num', instances should satisfy the following law: 

@
'falsy' = (0 ==) 
@


non-instances: 

* @()@ 
* @Dual@ 


-}
class Boolean a where

 falsy :: a -> Bool

 truthy :: a -> Bool
 truthy = not . falsy 


-- booleans 
instance Boolean Bool         where falsy = (== False)      
instance Boolean Any          where falsy = falsy . getAny  
instance Boolean All          where falsy = falsy . getAll  

-- errors   
instance Boolean (Maybe a)    where falsy = (\case Nothing -> True; _ -> False) -- not (== Nothing) 
instance Boolean (Either e a) where falsy = (\case Left{} -> True; _ -> False)        
-- | (includes 'String's)
instance Boolean [a]          where falsy = (\case [] -> True; _ -> False) -- not (== [])  

-- numbers 
instance Boolean Int          where falsy = (== 0)  
instance Boolean Word         where falsy = (== 0)  
instance Boolean Integer      where falsy = (== 0)  
instance Boolean Natural      where falsy = (== 0)  
instance (Integral a) => Boolean (Ratio a)    where falsy = (== 0)  
instance (RealFloat a) => Boolean (Complex a)  where falsy = (== 0)  

-- containers 
instance Boolean (Set a)      where falsy = Set.null  
instance Boolean (Map k v)    where falsy = Map.null  

-- strings 
instance Boolean Char         where falsy = (== '\NUL')       
instance Boolean Text         where falsy = Text.null         
instance Boolean ByteString   where falsy = ByteString.null   

-- et cetera 
instance Boolean ExitCode      where falsy = (\case ExitFailure{} -> True; _ -> False)  


instance (Foldable t)       => Boolean (WrappedFoldable t a) where falsy = null . toList  
instance (Monoid a, Eq a)   => Boolean (WrappedMonoid a)     where falsy = (mempty ==)    
instance (Num a,    Eq a)   => Boolean (WrappedNum a)        where falsy = (0 ==)         

newtype WrappedFoldable t a = WrappedFoldable { getWrappedFoldable :: t a } deriving (Foldable)
newtype WrappedMonoid   a   = WrappedMonoid   { getWrappedMonoid   :: a } deriving (Monoid,Eq)
newtype WrappedNum      a   = WrappedNum      { getWrappedNum      :: a } deriving (Num,Eq)



-- control 

whenB :: (Applicative m, Boolean b) => b -> m () -> m () 
whenB condition action = ifB condition action nothing
{-# INLINEABLE whenB #-}

whenM :: (Monad m, Boolean b) => m b -> m () -> m () 
whenM condition action = ifM condition action nothing 
{-# INLINEABLE whenM #-}

guardB :: (Alternative m, Boolean b) => b -> m ()
guardB condition = unlessB condition empty 
{-# INLINEABLE guardB #-}

guardM :: (MonadPlus m, Boolean b) => m b -> m ()
guardM condition = unlessM condition mzero
{-# INLINEABLE guardM #-}

unlessB :: (Applicative m, Boolean b) => b -> m () -> m () 
unlessB condition action = ifB condition nothing action 
{-# INLINEABLE unlessB #-}

unlessM :: (Monad m, Boolean b) => m b -> m () -> m () 
unlessM condition action = ifM condition nothing action 
{-# INLINEABLE unlessM #-}

boolB :: (Boolean b) => a -> a -> b -> a 
boolB x y c = ifB c x y 
{-# INLINEABLE boolB #-}

ifB :: (Boolean b) => b -> a -> a -> a 
ifB c x y = 
 if   truthy c 
 then x 
 else y 
{-# INLINEABLE ifB #-}

ifM :: (Monad m, Boolean b) => m b -> m a -> m a -> m a 
ifM condition a b = (\c -> ifB c a b) =<< condition 
{-# INLINEABLE ifM #-}

nothing :: (Applicative m) => m () 
nothing = pure()


{-$ alternatives
 
too explicit: <https://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Monad.html>

fewer instances: <https://hackage.haskell.org/package/cond> 

-}
