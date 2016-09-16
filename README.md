# Haskell Happstack Personal Website
This website is my attempt at creating a website and server in Haskell on Heroku. It uses @begriffs's Haskell buildpack and is largely inspired by https://github.com/tel/happstack-heroku-test

I plan for this website to become my own personal website but is it currently incomplete. Here is a list of things that have been achieved:

## Currently available

- Can output HTML using Blaze-Html with embedded CSS generated using Clay
- Has access to a MongoDB

## TODO

- Connect to the MongoDB and start using it to store data related to a blog/personal website
- Look into Javascript generating librairies
- Make the generated website pretty and usable

## Problems with unknown solutions

These are problems that I do not know how to solve. I do not plan to tackle these issues because they are not major problems and my limited search hasn't revealed a quick answer. Feel free to contact me about those.

- Separate CSS from the generated HTML so that the CSS is not embedded in the generated HTML. I will need a similar solution when I get to generating javascript. I could add static CSS and Javascript files, but I'd like to still be able to use Clay and any javascript-generating library I would want to use in the future.