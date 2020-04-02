let Language = ./lib/WTP/Language.dhall

let Text/concatSep = https://prelude.dhall-lang.org/Text/concatSep

in  { actions = [ Language.Action.Click (Language.css ".my-app form submit") ]
    , formula =
        Language.withOperators
          (   λ(Formula : Type)
            → λ(op : Language.Operators Formula)
            → let spinnerIsActive =
                      λ(active : Bool)
                    → op.match
                        (Language.css ".my-app .spinner")
                        Language.Attribute.ClassName
                        (if active then Some "active" else None Text)

              let hasMessage =
                      λ(message : Text)
                    → λ(classes : List Text)
                    → op.and
                        ( op.match
                            (Language.css ".my-app .message")
                            Language.Attribute.ClassName
                            (Some (Text/concatSep " " classes))
                        )
                        ( op.match
                            (Language.css ".my-app .message")
                            Language.Attribute.InnerText
                            (Some message)
                        )

              in  op.until
                    ( op.and
                        ( hasMessage
                            "Please fill out the form."
                            [ "message", "info" ]
                        )
                        (spinnerIsActive False)
                    )
                    ( op.until
                        (spinnerIsActive True)
                        ( op.always
                            ( op.and
                                ( hasMessage
                                    "Failed to post form."
                                    [ "message", "error" ]
                                )
                                (spinnerIsActive False)
                            )
                        )
                    )
          )
    }
