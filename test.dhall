let Language = ./lib/WebCheck.dhall

let Text/concatSep = https://prelude.dhall-lang.org/Text/concatSep

let assertThat = Language.assertThat

in  { actions = [ Language.Action.Click (Language.css ".my-app form submit") ]
    , formula =
        Language.withOperators
          (   λ(Formula : Type)
            → λ(op : Language.Operators Formula)
            → let spinnerIsActive =
                      λ(active : Bool)
                    → op.assertText
                        (Language.css ".my-app .spinner")
                        Language.Attribute.ClassName
                        (assertThat.text.equals "active")

              let hasMessage =
                      λ(message : Text)
                    → λ(classes : List Text)
                    → op.and
                        ( op.assertText
                            (Language.css ".my-app .message")
                            Language.Attribute.ClassName
                            (assertThat.text.equals (Text/concatSep " " classes))
                        )
                        ( op.assertText
                            (Language.css ".my-app .message")
                            Language.Attribute.InnerText
                            (assertThat.text.equals message)
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
