# General

This package present an alternative for the standard `yesod init`. It was modified
to my taste and needs and as such might not be suitable for your taste or needs.

The application contains simple login system utilizing `yesod-auth`.
I tried to avoid what other `Yesod` based sites that I found (e.g. `haskellers.com`) did with `yesod-auth`, that is they
wrapped almost all `User` (or similar type) fields in `Maybe` so that they could circumvent the limitations of the `yesod-auth`
implementation of third party authetication services that make implementing registration feel unnatural.
I did it by having separate `Identity` and `Profile` tables. Ideal solution would have these two tables merged (even though having them separate might be advatageous in some cases).
Let me know if you know how to achieve that with `yesod-auth` without resorting to `Maybe` hacks.



# About

  * `src/Main.hs`
    This module contains the main function.
    This is analogous to `devel.hs` + `main.hs` of the original `yesod init`.

  * `src/Site.hs`
    This is analogous to `Application.hs` of the original `yesod init`.

  * `src/Site/Common.hs`
    This module contains some convenience functions and reexpors some widely used types and functions.

  * `src/Site/Core.hs`
    This module contains definition of the underlying data type `Environment` and instances of the following
    classes: `Yesod`, `YesodAuth`, `YesodPersist`, `RenderMessage`.
    This is analogous to `Foundation.hs` of the original `yesod init`.

  * `src/Site/Settings.hs`
    This is analogous to `Settings.hs` of the original `yesod init`.

  * `src/Site/Static.hs`
    This is analogous to `Settings/Static.hs` of the original `yesod init`.

  * `src/Site/Model/Schema.hs`
    This module defines the schema of the database.
    This is analogous to `Model.hs` + `config/models` of the original `yesod init`.

  * `src/Site/Model/Type`
    This folder contains modules that reexport parts of `src/Site/Model/Schema.hs` for convenience.

  * `src/Site/Controller/Routes.hs`
    This module defines the routes of the application.
    This is analogous to `config/routes` of the original `yesod init`.

  * `src/Site/Controller/Form`
    This folder contains modules that define various forms.

  * `src/Site/Controller/Handler`
    This folder contains modules taht define the site's handlers and some convenience modules (namely `src/Site/Controller/Common.hs`).
