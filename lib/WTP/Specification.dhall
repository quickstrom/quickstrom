let Language = ./Language.dhall

let Specification =
      λ(Formula : Type) → { actions : List Language.Action, formula : Formula }

in Specification
