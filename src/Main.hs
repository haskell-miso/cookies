----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import           Miso.Cookie
import qualified Miso.CSS           as CSS
import           Miso.CSS.Color     (white, rgb, rgba)
import qualified Miso.Html          as H
import qualified Miso.Html.Property as P
import           Miso.Lens
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
data Model
  = Model
  { _inputName     :: MisoString
  , _inputValue    :: MisoString
  , _readResult    :: Maybe MisoString
  , _allCookies    :: [Cookie]
  , _statusMessage :: MisoString
  } deriving (Show, Eq)
----------------------------------------------------------------------------
inputName     :: Lens Model MisoString
inputName      = lens _inputName     $ \r f -> r { _inputName     = f }

inputValue    :: Lens Model MisoString
inputValue     = lens _inputValue    $ \r f -> r { _inputValue    = f }

readResult    :: Lens Model (Maybe MisoString)
readResult     = lens _readResult    $ \r f -> r { _readResult    = f }

allCookies    :: Lens Model [Cookie]
allCookies     = lens _allCookies    $ \r f -> r { _allCookies    = f }

statusMessage :: Lens Model MisoString
statusMessage  = lens _statusMessage $ \r f -> r { _statusMessage = f }
----------------------------------------------------------------------------
data Action
  = SetName    MisoString
  | SetValue   MisoString
  | DoGet
  | DoSet
  | DoDelete
  | GotCookie  (Maybe MisoString)
  | GotAll     [Cookie]
  | CookieErr  MisoString
  | Changed    CookieChangeEvent
  | Saved
  | Deleted
  deriving (Show, Eq)
----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel updateModel viewModel) {
    subs = [ cookieChangeSub Changed ]
  }
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model
  { _inputName     = "my-cookie"
  , _inputValue    = "hello"
  , _readResult    = Nothing
  , _allCookies    = []
  , _statusMessage = "Ready."
  }
----------------------------------------------------------------------------
updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  SetName  n  -> inputName  .= n
  SetValue v  -> inputValue .= v
  DoGet       -> do
    m <- get
    cookieGet (m ^. inputName) GotCookie CookieErr
  DoSet       -> do
    m <- get
    cookieSet (defaultCookie (m ^. inputName) (m ^. inputValue)) Saved CookieErr
  DoDelete    -> do
    m <- get
    cookieDelete (m ^. inputName) Deleted CookieErr
  GotCookie c -> do
    readResult    .= Just (maybe "(not found)" id c)
    statusMessage .= maybe "Cookie not found." (const "Cookie read.") c
    cookieGetAll GotAll CookieErr
  GotAll cs   ->
    allCookies .= cs
  CookieErr e ->
    statusMessage .= "Error: " <> e
  Saved       -> do
    statusMessage .= "Cookie saved."
    cookieGetAll GotAll CookieErr
  Deleted     -> do
    statusMessage .= "Cookie deleted."
    cookieGetAll GotAll CookieErr
  Changed ev  -> do
    io_ $ consoleLog $
      ms (length (cookiesChanged ev)) <> " changed, " <>
      ms (length (cookiesDeleted ev)) <> " deleted (change event)."
    cookieGetAll GotAll CookieErr
----------------------------------------------------------------------------
viewModel :: props -> Model -> View Model Action
viewModel _ m =
  H.div_
    [ CSS.style_
        [ CSS.fontFamily      "system-ui, sans-serif"
        , CSS.maxWidth        (CSS.px 560)
        , CSS.margin          "40px auto"
        , CSS.padding         (CSS.px 24)
        , CSS.backgroundColor (rgb 245 247 250)
        , CSS.borderRadius    (CSS.px 12)
        ]
    ]
    [ heading
    , inputRow "Cookie name"  (m ^. inputName)  SetName
    , inputRow "Cookie value" (m ^. inputValue) SetValue
    , buttonRow
    , statusBar m
    , readResultView m
    , allCookiesView m
    ]
----------------------------------------------------------------------------
heading :: View Model Action
heading =
  H.div_ []
    [ H.h2_
        [ CSS.style_
            [ CSS.margin   "0 0 8px"
            , CSS.fontSize (CSS.rem 1.4)
            , CSS.color    (rgb 30 30 50)
            ]
        ]
        [ text "CookieStore API demo" ]
    , H.p_
        [ CSS.style_
            [ CSS.margin   "0 0 20px"
            , CSS.fontSize (CSS.rem 0.88)
            , CSS.color    (rgb 100 100 130)
            ]
        ]
        []
    ]
----------------------------------------------------------------------------
inputRow
  :: MisoString
  -> MisoString
  -> (MisoString -> Action)
  -> View Model Action
inputRow label_ val action =
  H.div_
    [ CSS.style_
        [ CSS.display      "flex"
        , CSS.alignItems   "center"
        , CSS.gap          (CSS.px 12)
        , CSS.marginBottom (CSS.px 12)
        ]
    ]
    [ H.label_
        [ CSS.style_ [ CSS.width (CSS.px 110), CSS.color (rgb 80 80 100) ] ]
        [ text label_ ]
    , H.input_
        [ P.type_    "text"
        , P.value_   val
        , H.onInput  action
        , CSS.style_
            [ CSS.flexGrow     1
            , CSS.padding      "6px 10px"
            , CSS.border       "1px solid #ccc"
            , CSS.borderRadius (CSS.px 6)
            , CSS.fontSize     (CSS.rem 0.95)
            ]
        ]
    ]
----------------------------------------------------------------------------
buttonRow :: View Model Action
buttonRow =
  H.div_
    [ CSS.style_
        [ CSS.display  "flex"
        , CSS.gap      (CSS.px 8)
        , CSS.flexWrap "wrap"
        , CSS.margin   "16px 0"
        ]
    ]
    [ btn "Get"    (rgb  59 130 246) DoGet
    , btn "Set"    (rgb  34 197  94) DoSet
    , btn "Delete" (rgb 239  68  68) DoDelete
    ]
----------------------------------------------------------------------------
btn :: MisoString -> CSS.Color -> Action -> View Model Action
btn label_ color_ action =
  H.button_
    [ H.onClick action
    , CSS.style_
        [ CSS.padding         "8px 18px"
        , CSS.backgroundColor color_
        , CSS.color           white
        , CSS.border          "none"
        , CSS.borderRadius    (CSS.px 6)
        , CSS.cursor          "pointer"
        , CSS.fontSize        (CSS.rem 0.9)
        , CSS.fontWeight      "600"
        ]
    ]
    [ text label_ ]
----------------------------------------------------------------------------
statusBar :: Model -> View Model Action
statusBar m =
  H.p_
    [ CSS.style_
        [ CSS.margin          "0 0 12px"
        , CSS.padding         "10px 14px"
        , CSS.backgroundColor (rgba 200 220 255 0.4)
        , CSS.borderRadius    (CSS.px 6)
        , CSS.fontSize        (CSS.rem 0.9)
        , CSS.color           (rgb 40 60 120)
        ]
    ]
    [ text (m ^. statusMessage) ]
----------------------------------------------------------------------------
readResultView :: Model -> View Model Action
readResultView m =
  case m ^. readResult of
    Nothing -> H.div_ [] []
    Just v  ->
      H.div_
        [ CSS.style_
            [ CSS.marginBottom    (CSS.px 12)
            , CSS.padding         "10px 14px"
            , CSS.backgroundColor white
            , CSS.borderRadius    (CSS.px 6)
            , CSS.border          "1px solid #dde"
            ]
        ]
        [ H.strong_ [] [ text "Last read: " ]
        , text v
        ]
----------------------------------------------------------------------------
allCookiesView :: Model -> View Model Action
allCookiesView m
  | null (m ^. allCookies) = H.div_ [] []
  | otherwise =
      H.div_ []
        ( H.p_
            [ CSS.style_ [ CSS.fontWeight "600", CSS.marginBottom (CSS.px 6) ] ]
            [ text "All cookies:" ]
        : map cookieRow (m ^. allCookies)
        )
----------------------------------------------------------------------------
cookieRow :: Cookie -> View Model Action
cookieRow c =
  H.div_
    [ CSS.style_
        [ CSS.display         "flex"
        , CSS.gap             (CSS.px 8)
        , CSS.padding         "6px 10px"
        , CSS.marginBottom    (CSS.px 4)
        , CSS.backgroundColor white
        , CSS.borderRadius    (CSS.px 6)
        , CSS.border          "1px solid #dde"
        , CSS.fontSize        (CSS.rem 0.88)
        ]
    ]
    [ H.strong_ [] [ text (cookieName c) ]
    , text "="
    , text (maybe "" id (cookieValue c))
    ]
----------------------------------------------------------------------------
