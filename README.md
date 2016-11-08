# Haskell Happstack Personal Website
This website is my attempt at creating a website and server in Haskell on Heroku. It uses @begriffs's Haskell buildpack available here : https://github.com/begriffs/heroku-buildpack-ghc

## Building on a local machine

To build, you need to install ghc and cabal. 

1. Fork and clone
2. Enter this in a console:

`
cabal sandbox init
cabal install --only-dependencies
cabal build
`

This will create a sandbox inside the project's directory and will install all dependencies from Hackage. Finally, the last command will build the program in `./dist/build/server/server`. You might need to run `cabal update` the first time you run cabal. After any modifications, you only need to run `cabal build` to build the project.

## Launch on Heroku

1. Fork and clone
2. Enter this in a console:

`
heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
git push heroku master
`

This will create a new website at a new random address generated by Heroku and push the project to it.


## Currently available

I plan for this website to become my own personal website but is it currently incomplete. Here is a list of things that have been achieved:

- Can output HTML using Blaze-Html with embedded CSS generated using Clay
- Has access to a MongoDB
- Connects to the MongoDB and uses it to store data related to a blog/personal website
- Converts markdown from the database to HTML 

## TODO

- Make the generated website pretty and usable
- Look into Javascript generating libraries

## Problems with unknown solutions

These are problems that I do not know how to solve. I do not plan to tackle these issues because they are not major problems and my limited search hasn't revealed a quick answer. Feel free to contact me about those.

- Separate CSS from the generated HTML so that the CSS is not embedded in the generated HTML. I will need a similar solution when I get to generating javascript. I could add static CSS and Javascript files, but I'd like to still be able to use Clay and any javascript-generating library I would want to use in the future.
