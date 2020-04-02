let Language = ./lib/WTP/Language.dhall

let Specification = ./lib/WTP/Specification.dhall

let Main = ./lib/WTP/Main.dhall

in    λ(Formula : Type)
    → λ(Fix : Language.FormulaF Formula → Formula)
    → Main.specify
        Formula
        Fix
        (   λ(op : Language.Operators Formula)
          → op.not op.bottom
        )
