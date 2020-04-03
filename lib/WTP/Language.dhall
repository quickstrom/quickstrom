let Selector
    : Type
    = { selector : Text }

let css
    : Text → Selector
    = λ(selector : Text) → { selector }

let Action
    : Type
    = < Click : Selector | Refresh >

let Attribute
    : Type
    = < InnerHTML | InnerText | ClassName >

let TextAssertion
    : Type
    = < TextEquals : Text | TextContains : Text >

let FormulaF =
        λ(Formula : Type)
      → < TrueF
        | NotF : Formula
        | OrF : { _1 : Formula, _2 : Formula }
        | UntilF : { _1 : Formula, _2 : Formula }
        | AssertTextF : { _1 : Selector, _2 : Attribute, _3 : TextAssertion }
        >

let Operators
    : ∀(Formula : Type) → Type
    =   λ(Formula : Type)
      →   { true : Formula
          , not : Formula → Formula
          , or : Formula → Formula → Formula
          , until : Formula → Formula → Formula
          , assertText : Selector → Attribute → TextAssertion → Formula
          }
        ⩓ { false : Formula
          , and : Formula → Formula → Formula
          , implies : Formula → Formula → Formula
          , equivalent : Formula → Formula → Formula
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

        let assertText =
                λ(selector : Selector)
              → λ(attribute : Attribute)
              → λ(assertion : TextAssertion)
              → Fix
                  ( T.AssertTextF
                      { _1 = selector, _2 = attribute, _3 = assertion }
                  )

        let atoms = { true, not, or, until, assertText }

        let false = not true

        let and = λ(x : Formula) → λ(y : Formula) → not (or (not x) (not y))

        let implies = λ(x : Formula) → λ(y : Formula) → or (not x) y

        let equivalent =
              λ(x : Formula) → λ(y : Formula) → and (implies x y) (implies y x)

        let release =
              λ(x : Formula) → λ(y : Formula) → not (until (not x) (not y))

        let eventually = λ(x : Formula) → until true x

        let always = λ(x : Formula) → not (eventually (not x))

        let derived =
              { false, and, implies, equivalent, release, eventually, always }

        in  atoms ∧ derived : Operators Formula

let withOperators =
        λ(f : ∀(F : Type) → Operators F → F)
      → λ(Formula : Type)
      → λ(Fix : FormulaF Formula → Formula)
      → f Formula (operators Formula Fix)

let assertThat = { text.equals = TextAssertion.TextEquals }

in  { css
    , Attribute
    , Action
    , assertThat
    , FormulaF
    , Operators
    , operators
    , withOperators
    }
