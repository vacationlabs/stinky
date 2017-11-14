module Stinky
  (
    module Stinky
  , module Lucid
  )
where

import Lucid hiding (button_)
import Lucid.Base hiding (button_)
import Data.List as DL
import Data.Text as T
import Text.InterpolatedString.Perl6
import Data.Monoid ((<>))

-- TODO: What is the most sensible URL library out there? We should pick this
-- data-type from that library.
type URL = Text

type Subject = Text

align_ :: Text -> Attribute
align_ = makeAttribute "align"

valign_ :: Text -> Attribute
valign_ = makeAttribute "valign"

center_ :: Term arg result => arg -> result
center_ = term "center"

hasClass :: Text -> [Attribute] -> Bool
hasClass target attrs = DL.any (\(Attribute k v) -> k=="class" && (T.isInfixOf target v)) attrs


container_ :: (Monad m) => HtmlT m () -> HtmlT m ()
container_ arg = table_ [align_ "center", class_ " container "] $ do
  tbody_ $ do
    tr_ $ do
      td_ arg

row_ :: (Monad m) => HtmlT m () -> HtmlT m ()
row_ arg = table_ [class_ " row "] $ do
  tbody_ $ do
    tr_ arg


columns_ :: (Monad m) => HtmlT m () -> HtmlT m ()
columns_ arg = th_ [class_ " columns "] $ do
  table_ $ do
    tr_ $ do
      th_ arg
      th_ [class_ "expander"] ""

-- TODO: automatically adding firsta & last classes

centerImg_ :: (Monad m) => [Attribute] -> HtmlT m ()
centerImg_ attrs = center_ $ do
  img_ (attrs ++ [align_ "center"] ++ withCentered)
  where
    withCentered = if (hasClass "small-float-center" attrs)
                   then []
                   else [class_ " float-center "]

-- TODO: centered button
button_ :: (Monad m) => URL -> [Attribute] -> HtmlT m () -> HtmlT m ()
button_ url attrs inner = with (table_ [class_ " button "]) attrs $ do
  tr_ $ do
    td_ $ do
      table_ $ do
        tr_ $ do
          td_ $ do
            a_ [href_ url] inner

centerButton_ :: (Monad m) => URL -> [Attribute] -> HtmlT m () -> HtmlT m ()
centerButton_ url attrs inner = center_ $ do
  with (button_ url attrs) [class_ " float-center "] inner

expandedButton_ :: (Monad m) => URL -> HtmlT m () -> HtmlT m ()
expandedButton_ url inner = table_ [class_ " button expanded "] $ do
  tr_ $ do
    td_ $ do
      table_ $ do
        tr_ $ do
          td_ $ do
            center_ $ do
              a_ [href_ url, align_ "center", class_ " float-center "] inner

  

-- TODO: centering menus


wrapper_ :: (Monad m) => HtmlT m () -> HtmlT m ()
wrapper_  arg  = table_ [class_ " wrapper ", align_ "center"] $ do
  tr_ $ do
    td_ [class_ " wrapper-inner "] arg

spacer_ :: (Monad m) => Int -> HtmlT m ()
spacer_ size = table_ [class_ " spacer "] $ do
  tbody_ $ do
    tr_ $ do
      td_ ([height_ [qc|{size}px|], style_ [qc|font-size:{size}px; line-height:{size}px;|]]) ""

customHr_ :: (Monad m) => HtmlT m ()
customHr_ =   row_ (with columns_ [class_ " first last "] (hr_ []))

type Preheader = Text
type InlineStyles = Text
emailLayout_ :: (Monad m) => Text -> Subject -> Preheader -> HtmlT m () -> HtmlT m ()
emailLayout_ inlineStyles subject preheader bodyContent = doctypehtml_ $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width"]
    title_ (toHtml subject)
    style_ inlineStyles
  body_ $ do
    span_ [class_ " preheader "] (toHtml preheader)
    table_ [class_ " body "] $ do
      tr_ $ do
        td_ [class_ " center ", align_ "center", valign_ "top"] $ do
          center_ bodyContent

singleColumn_ :: (Monad m) => HtmlT m () -> HtmlT m ()
singleColumn_ inner = row_ $ do
  with columns_ [class_ " first last "] inner

firstColumn_ :: (Monad m) => HtmlT m () -> HtmlT m ()
firstColumn_ inner = with columns_ [class_ " first "] inner

lastColumn_ :: (Monad m) => HtmlT m () -> HtmlT m ()
lastColumn_ inner = with columns_ [class_ " last "] inner


-- sendTestEmail :: (HasSendgrid m, HasLogging m) => Text -> m ()
-- sendTestEmail email_ = do
--   finalCss_ <- textReadFile "email-templates/email.css"
--   t <- getCurrentUtcTime
--   let emailContent_ = renderText $ email finalCss_
--   resp <- httpPostJSON "http://localhost:3000/inline-css" $ object ["html" .= emailContent_]
--   let ms = SG.MailSend
--         { SG._mlsdPersonalizations = [
--             SG.Personalization
--               { _prsTo = [SG.SendgridEmailAddress (Just "Recipient name comes here") email_]
--               , _prsSubject = Nothing
--               , _prsSubstitutions = Nothing
--               }
--             ]
--         , SG._mlsdFrom = SG.SendgridEmailAddress (Just "Vacation Labs") "services@vacationlabs.com"
--         , SG._mlsdSubject = Just ([qc|Test payment link @ {t}|])
--         , SG._mlsdContent = [SG.MimeContent "text/html" (toS $ resp ^. W.responseBody)]
--         , SG._mlsdAttachments = Nothing
--         , SG._mlsdTemplate_id  = Nothing
--         , SG._mlsTracking_settings = Nothing
--         , SG._mlsCustom_args = Nothing
--         }
--   apiKey_ <- getAppSendgridConfig
--   SG.mailSend apiKey_ ms
--   pure ()

callout_ :: (Monad m) => Text -> HtmlT m () -> HtmlT m ()
callout_ calloutClass inner = table_ [class_ " callout "] $ do
  tr_ $ do
    th_ [class_ $ " callout-inner " <> calloutClass <> " "] inner
