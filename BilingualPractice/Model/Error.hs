module BilingualPractice.Model.Error where


data Error = NoDataError | InconsistentTraversalError deriving (Enum, Bounded, Ord, Eq, Show)
