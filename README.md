snap + web-routes
=================

This demo modifies the project created by `snap init default` to include support for web-routes. It requires no modifications to snap, heist, or web-routes. Though, there is a bit of boilerplate that could be moved into a web-routes-snap package.

src/Application.hs now includes a type-safe URL:

    data AppURL
        = Count Int
          deriving (Eq, Ord, Read, Show, Data, Typeable)
    
    $(derivePathInfo ''AppURL)

For simplicity, we use Template Haskell to automatically derive the mapping between the URL type and the URL string. But we could use boomerang, quasiquotes, or an number of methods to define that mapping.

Application.hs also defines some MonadRoute instances and a new function:

    instance MonadRoute (Handler App App)
    instance (MonadRoute m) => MonadRoute (HeistT m)

    heistURL :: MonadRoute m => URL m -> m [Node]

The index.tpl now includes an additional link:

         <a href="${countURL}">count</a>

In src/Site.hs in the index function we bind countURL to the type-safe URL via:

        , ("countURL"     , heistURL (Count 10))

This is just a quick little hack. The `MonadRoute (HeistT m)` instance should certainly be put in a library. 

It might be reasonable to extend heist so that you can use the type-safe URL directly like:


         <a href="#{Count 10}">count</a>

That solution is actually less type-safe in most cases, since the template files are generally parse and runtime. But, it should still be able to provide a run-time warning.
