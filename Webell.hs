{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Webell where

    import Text.Printf
    import Language.Haskell.Interpreter

    data Tag a where
        --                          Tag       Options      Children
        Tag         :: (HTML a) => String -> [TagOption] -> [Tag a] -> Tag a
        SelfClosing :: (HTML a) => String -> [TagOption] -> Tag a
        Value       :: (HTML a) => a -> Tag a
        Hask        :: String -> Tag String

    data TagOption where
        TO :: String -> String -> TagOption
        Lone :: String -> TagOption
        deriving Show


    tagMeta :: TagOption -> String
    tagMeta (Lone left)    = printf "%s" left
    tagMeta (TO left right) = printf " %s=\"%s\"" left right

    metaString :: [TagOption] -> String
    metaString = concatMap tagMeta

    -- Make custom type class

    class HTML a where
        toHTML :: a -> String
        default toHTML :: Show a => a -> String
        toHTML = show

    instance HTML (Tag a) where
        toHTML (Tag nm args val)        = printf "<%s%s> %s </%s>" nm (metaString args) (concatMap toHTML val) nm
        toHTML (SelfClosing nm args)    = printf "<%s%s />" nm (metaString args)
        toHTML (Value value)            = printf "%s" (toHTML value)
        toHTML (Hask expression)        = expression

    instance HTML Int
    instance HTML Double
    instance HTML Float
    instance HTML Integer

    instance HTML String where
        toHTML = id


    instance HTML (Either InterpreterError (Tag String)) where
      toHTML l = case l of
        Left err -> show err
        Right ts -> toHTML ts

    metaLess :: HTML a => ([TagOption] -> a -> Tag a) -> a -> Tag a
    metaLess func = func []

    {- Select Tags -}

    option :: HTML a => [TagOption] -> a -> Tag a
    option ops value = Tag "option" ops [Value value]

    select :: HTML a =>[TagOption] -> [a] -> Tag String
    select ops items = Tag "select" ops (metaLess option <$> map toHTML items)

    {- List Tags -}

    listItem :: HTML a => [TagOption] -> a -> Tag a
    listItem ops val = Tag "li" ops [Value val]

    orderedList :: HTML a => [TagOption] -> [a] -> Tag a
    orderedList ops items = Tag "ol" ops (metaLess listItem <$> items)

    unorderedList :: HTML a => [TagOption] -> [a] -> Tag a
    unorderedList ops items = Tag "ul" ops (metaLess listItem <$> items)

    {- Form Generation -}

    formInput :: HTML a => [TagOption] -> Tag a
    formInput = SelfClosing "input"

    form :: HTML a => [TagOption] -> [Tag a] -> Tag a
    form = Tag "form"

    {- Misc Tags -}

    img :: HTML a => String -> Tag a -> [TagOption] -> Tag a
    img link content options = Tag "img" (TO "src" link : options) [content]

    h :: Integer -> [TagOption] -> String -> Tag String
    h level ops content = Tag (printf "h%d" level) ops [Value content]

    p :: String -> [TagOption] -> Tag String
    p content ops = Tag "p" ops [Value content]

    a :: [TagOption] -> String -> String -> Tag String
    a ops text link = Tag "a" (TO "href" link : ops) [Value text]

    divTag :: HTML a => [TagOption] -> [Tag a] -> Tag a
    divTag = Tag "div"

    {- Table Generation -}

    tr :: HTML a => [Tag a] -> [TagOption] -> Tag a
    tr  content ops = Tag "tr" ops content

    td :: HTML a => [TagOption] -> a -> Tag a
    td ops content = Tag "td" ops [Value content]


    tableRow :: HTML a => [TagOption] -> [a] -> Tag String
    tableRow options items = tr (metaLess td <$> (toHTML <$> items)) options

    table :: HTML a => [[a]] -> [TagOption] -> Tag String
    table row_col ops = Tag "table" ops (tableRow [] <$> row_col)

    {- Niceties -}

    br :: Tag String
    br = SelfClosing "br" []


-- h 1 [TO ("class", "myclass")] "My Header"
-- img "http://google.com" (Value "My Link Here") [TO ("class", "some long class string")]
-- p "Some String here" [TO ("class", "some other class string")]
-- divTag [TO ("My Single tag", [])]
