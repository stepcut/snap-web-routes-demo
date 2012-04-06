snap + web-routes
=================

This demo modifies the project created by `snap init default` to include support for web-routes. It requires no modifications to snap, heist, or web-routes. Though, there is a bit of boilerplate that could be moved into a web-routes-snap package.

src/Application.hs now includes a type-safe URL:

    data AppURL
        = Count Int
          deriving (Eq, Ord, Read, Show, Data, Typeable)
    
    $(derivePathInfo ''AppURL)

For simplicity, we use Template Haskell to automatically derive the mapping between the URL type and the URL string. But we could use boomerang, quasiquotes, or an number of methods to define that mapping.

The `App` type is extended to hold the function which shows the route:

    data App = App
        { _heist     :: Snaplet (Heist App)
        , _startTime :: UTCTime
        , _routeFn   :: AppURL -> [(Text, Maybe Text)] -> Text
        }
Application.hs also defines some `MonadRoute` instances and two new functions:

    instance MonadRoute (Handler App App)
    instance (MonadRoute m) => MonadRoute (HeistT m)

    heistURL :: MonadRoute m => URL m -> m [Node]
    webRoute :: (PathInfo url, MonadSnap m) => (url -> m ()) -> m ()

The index.tpl now includes an additional link:

         <a href="${countURL}">count</a>

In src/Site.hs in the `index` function we bind `countURL` to the type-safe URL via:

        , ("countURL"     , heistURL (Count 10))

In the `app` function we include some code to initialize the `_routeFn` field:

    return $ App h sTime (\u p -> "/wr" `T.append` toPathInfoParams u p)

We put all the web-route routes under `"/wr"` so that they do not collide with other URLs.

In the `routes` function we add a new handler to handle the routes defined by web-routes:

         , ("wr", webRoute routeAppURL)

the `routeAppURL` function just uses pattern matching to map the `AppURL` to a handler:

    routeAppURL :: MonadSnap m => AppURL -> m ()
    routeAppURL appURL =
        case appURL of
          (Count n) -> writeText $ ("Count = " `T.append` (T.pack $ show n))

This is just a quite little demo, but I think it shows that using web-routes with snap and heist should be pretty straight-forward.

The `MonadRoute (HeistT m)` instance, and the `heistURL` and `webRoute` functions could certainly be put in a library.

It might be reasonable to extend heist so that you can use the type-safe URL directly like:

         <a href="#{Count 10}">count</a>

That solution is actually less type-safe in most cases, since the template files are generally parsed at runtime. But, it should still be able to provide a run-time warning.

