# Haskell Happstack Personal Website
This website is my attempt at creating a website and server in Haskell on Heroku. It uses @begriffs's Haskell buildpack and is largely inspired by https://github.com/tel/happstack-heroku-test

I plan for this website to become my own personal website but is it currently incomplete. Here is a list of things that have been achieved:

## Currently available

- Can output HTML using Blaze-Html with embedded CSS generated using Clay
- Has access to a MongoDB

## TODO

- Separate HTML generating code from anything to do with Happstack
- Connect to the MongoDB and start using it as a blog/personal website
- Look into Javascript generating librairies
- Make the generated website pretty and usable

## Problems with unknown solutions

These are problems that I do not know how to solve. I do not plan to tackle these issues because they are not major problems and my limited search hasn't revealed a quick answer. Feel free to contact me about those.

- Separate CSS from the generated HTML so that CSS so it's not embedded in the generated HTML. -- Will be useful if I ever get to generating Javascript
