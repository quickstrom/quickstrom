let Language = ./lib/WTP/Language.dhall

let Specification = ./lib/WTP/Specification.dhall

in  { actions =
          [ Language.Action.Click { selector = "form submit" } ]
        : List Language.Action
    , formula =
          λ(Formula : Type)
        → λ(Fix : Language.FormulaF Formula → Formula)
        → let op = Language.operators Formula Fix

          in  op.until
                (op.and op.bottom op.top)
                (op.or op.top (op.not op.bottom))
    }
