module WebCheck.Result where

import Algebra.Heyting
import Algebra.Lattice

data Result
  = Accepted
  | Rejected
  deriving (Eq, Show)

instance Lattice Result where
  Accepted /\ Accepted = Accepted
  _ /\ _ = Rejected

  Accepted \/ _ = Accepted
  Rejected \/ r = r

instance BoundedJoinSemiLattice Result where
  bottom = Rejected

instance BoundedMeetSemiLattice Result where
  top = Accepted

instance Heyting Result where
  Rejected ==> _ = Accepted
  Accepted ==> r = r
