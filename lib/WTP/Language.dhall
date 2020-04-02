let Action
    : Type
    = < Click : { selector : Text } | Refresh >

let FormulaF =
        λ(Formula : Type)
      → < TopF
        | BottomF
        | NotF : Formula
        | AndF : { _1 : Formula, _2 : Formula }
        | OrF : { _1 : Formula, _2 : Formula }
        | UntilF : { _1 : Formula, _2 : Formula }
        >

let Operators
    : ∀(Formula : Type) → Type
    =   λ(Formula : Type)
      → { top : Formula
        , bottom : Formula
        , not : Formula → Formula
        , and : Formula → Formula → Formula
        , or : Formula → Formula → Formula
        , until : Formula → Formula → Formula
        }

let operators =
        λ(Formula : Type)
      → λ(Fix : FormulaF Formula → Formula)
      → let T = FormulaF Formula

        in    { top = Fix T.TopF
              , bottom = Fix T.BottomF
              , not = λ(x : Formula) → Fix (T.NotF x)
              , and =
                    λ(x : Formula)
                  → λ(y : Formula)
                  → Fix (T.AndF { _1 = x, _2 = y })
              , or =
                    λ(x : Formula)
                  → λ(y : Formula)
                  → Fix (T.OrF { _1 = x, _2 = y })
              , until =
                    λ(x : Formula)
                  → λ(y : Formula)
                  → Fix (T.UntilF { _1 = x, _2 = y })
              }
            : Operators Formula

let withOperators =
        λ(f : ∀(F : Type) → Operators F → F)
      → λ(Formula : Type)
      → λ(Fix : FormulaF Formula → Formula)
      → f Formula (operators Formula Fix)

in  { Action, FormulaF, Operators, operators, withOperators }
