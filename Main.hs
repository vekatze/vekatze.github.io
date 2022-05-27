import Control.Applicative (Alternative (empty))
import Data.Aeson
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KM
import Hakyll
import Text.Pandoc hiding (getCurrentTimeZone)
import Prelude

main :: IO ()
main = do
  hakyllWith config $ do
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "post/*"
        let indexContext = listField "post" siteContext (return posts) <> siteContext
        getResourceBody
          >>= applyAsTemplate indexContext
          >>= loadAndApplyTemplate "template/content.html" indexContext
          >>= loadAndApplyTemplate "template/base.html" indexContext
    match "post/*" $ do
      route $ setExtension "html"
      compile $
        postCompiler
          >>= loadAndApplyTemplate "template/content.html" siteContext
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "template/base.html" siteContext
    match "asset/*" $ do
      route idRoute
      compile copyFileCompiler
    match "media/*" $ do
      route idRoute
      compile copyFileCompiler
    match "main.js" $ do
      route idRoute
      compile copyFileCompiler
    match "style.css" $ do
      route idRoute
      compile compressCssCompiler
    match (fromList ["favicon.ico"]) $ do
      route idRoute
      compile copyFileCompiler
    match "template/*" $
      compile templateBodyCompiler
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedContext = siteContext <> bodyField "description"
        posts <- recentFirst =<< loadAllSnapshots "post/*" "content"
        renderAtom feedConfiguration feedContext posts

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = siteTitle,
      feedDescription = "私の時計で始めます。",
      feedAuthorName = "vekatze",
      feedAuthorEmail = "vekatze@icloud.com",
      feedRoot = "https://vekatze.github.io"
    }

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

siteContext :: Context String
siteContext =
  mconcat
    [ constField "site-title" siteTitle,
      mathContext,
      audioContext,
      coverContext,
      metadataField,
      dateField "published" "%Y-%m-%d",
      bodyField "body",
      urlField "url"
    ]

siteTitle :: String
siteTitle =
  "以析比域"

fieldContext :: FromJSON a => String -> (a -> Compiler String) -> Context String
fieldContext fieldName valueModifier =
  field fieldName $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    case KM.lookup (fromString fieldName) metadata of
      Just rawValue
        | Success value <- fromJSON rawValue -> do
          valueModifier value
      _ ->
        noResult fieldName

mathContext :: Context String
mathContext = do
  let fieldName = "mathjax"
  fieldContext fieldName $ \flag ->
    if flag
      then return empty
      else noResult fieldName

audioContext :: Context String
audioContext =
  fieldContext "audio" $ \fileName ->
    return $ "/media/" <> fileName

coverContext :: Context String
coverContext =
  fieldContext "cover" $ \fileName ->
    return $ "/media/" <> fileName

postCompiler :: Compiler (Item String)
postCompiler = do
  getResourceBody
    >>= withItemBody (return . ("#+OPTIONS: \\n:t\n" <>))
    >>= renderPandocWith readerOptions writerOptions

extensions :: Extensions
extensions =
  extensionsFromList
    [ Ext_hard_line_breaks,
      Ext_markdown_in_html_blocks
    ]

readerOptions :: ReaderOptions
readerOptions = do
  defaultHakyllReaderOptions
    { readerExtensions = extensions
    }

writerOptions :: WriterOptions
writerOptions =
  defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax "",
      writerTemplate = Nothing,
      writerExtensions = extensions
    }
