let Selector
    : Type
    = { selector : Text }

let css: Text -> Selector = λ(selector : Text) → { selector }

let Action
    : Type
    = < Click : Selector | Refresh >

let FormulaF =
        λ(Formula : Type)
      → < TrueF
        | NotF : Formula
        | OrF : { _1 : Formula, _2 : Formula }
        | UntilF : { _1 : Formula, _2 : Formula }
        >

let Operators
    : ∀(Formula : Type) → Type
    =   λ(Formula : Type)
      →   { true : Formula
          , not : Formula → Formula
          , or : Formula → Formula → Formula
          , until : Formula → Formula → Formula
          }
        ⩓ { false : Formula
          , and : Formula → Formula → Formula
          , if : Formula → Formula → Formula
          , iff : Formula → Formula → Formula
          , release : Formula → Formula → Formula
          , eventually : Formula → Formula
          , always : Formula → Formula
          }

let operators =
        λ(Formula : Type)
      → λ(Fix : FormulaF Formula → Formula)
      → let T = FormulaF Formula

        let true = Fix T.TrueF

        let not = λ(x : Formula) → Fix (T.NotF x)

        let or =
              λ(x : Formula) → λ(y : Formula) → Fix (T.OrF { _1 = x, _2 = y })

        let until =
                λ(x : Formula)
              → λ(y : Formula)
              → Fix (T.UntilF { _1 = x, _2 = y })

        let atoms = { true, not, or, until }

        let false = not true

        let and = λ(x : Formula) → λ(y : Formula) → not (or (not x) (not y))

        let `if` = λ(x : Formula) → λ(y : Formula) → or (not x) y

        let iff = λ(x : Formula) → λ(y : Formula) → and (`if` x y) (`if` y x)

        let release =
              λ(x : Formula) → λ(y : Formula) → not (until (not x) (not y))

        let eventually = λ(x : Formula) → until true x

        let always = λ(x : Formula) → not (eventually (not x))

        let derived = { false, and, if, iff, release, eventually, always }

        in  atoms ∧ derived : Operators Formula

let withOperators =
        λ(f : ∀(F : Type) → Operators F → F)
      → λ(Formula : Type)
      → λ(Fix : FormulaF Formula → Formula)
      → f Formula (operators Formula Fix)

in  { css, Action, FormulaF, Operators, operators, withOperators }
