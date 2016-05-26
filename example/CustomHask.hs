module CustomHask where
    import           Webell
    import           Text.Printf

    mdl_card :: String -> String -> [(String, String)] -> Tag String
    mdl_card title body actions = divTag [TO "class" "demo-card-wide mdl-card mdl-shadow--4dp"]
                                  [
                                    mdl_card_head title,
                                    mdl_card_body body,
                                    mdl_card_action_section actions
                                  ]

    mdl_card_head :: String -> Tag String
    mdl_card_head title = divTag [TO "class" "mdl-card__title"]
                            [
                                h 2 [TO "class" "mdl-card__title-text"] title
                            ]
    mdl_card_body :: String -> Tag String
    mdl_card_body text = divTag [TO "class" "mdl-card__supporting-text"]
                        [
                            Value text
                        ]

    mdl_card_action_section :: [(String, String)] -> Tag String
    mdl_card_action_section actions = divTag [TO "class" "mdl-card__actions mdl-card--border"] (mdl_card_action <$> actions)

    mdl_card_action :: (String, String) -> Tag String
    mdl_card_action (text, link) = a [mdl_card_action_options] text link

    mdl_card_action_options :: TagOption
    mdl_card_action_options = TO "class" "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"


    bootstrap_column :: String -> Int -> TagOption
    bootstrap_column size width = TO "class" (printf "col-%s-%d" size width)
