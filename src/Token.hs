module Token where


data Token  = Atom String
            | Var String
            | If
            | Comma
            | Period
            | Paren'Open
            | Paren'Close
            | Underscore
            | Equal
            | EOF
  deriving (Eq, Show)
