{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
-- |
module Graphics.Rendering.ChartB.Types.Property
  ( Property(..)
    -- * Helpers
  , endoL
  , nonProp
  ) where

import Control.Lens
import Control.Category
import Data.Distributive
import Data.Maybe
import Data.Monoid
import Prelude hiding ((.),id)
import GHC.OverloadedLabels (IsLabel(..))


-- | Property which is just a lens 'Setter' in disguise. Newtype
--   wrapper is needed in order to be able to define 'IsLabel'
--   instances.
newtype Property a s = Property { prop :: Setter' s a }

instance Category Property where
  id = id
  Property f . Property g = Property (f . g)


instance IsLabel l (Property p a) => IsLabel l (Property p (Endo a)) where
  fromLabel = Property endoL . fromLabel @l

endoL :: Setter' (Endo a) a
endoL fun (Endo f) = fmap Endo $ distribute $ fun . f

nonProp :: a -> Property a (Maybe a)
nonProp x0 = Property $ \f -> fmap Just . f . fromMaybe x0
