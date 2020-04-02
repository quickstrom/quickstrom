let Language = ./Language.dhall

let Specification = ./Specification.dhall

in  { specify =
          λ(Formula : Type)
        → λ(Fix : Language.FormulaF Formula → Formula)
        → λ(mkSpec : Language.Operators Formula → Specification Formula)
        → mkSpec (Language.operators Formula Fix)
    }
