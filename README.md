# A Graphical Playground for Haskell
This project was created as part of my dissertation at The University of Edinburgh.
[Click here](https://haskell-playground.co.uk) to visit the website.
[Click here](https://haskell-playground.co.uk/dissertation) to read the dissertation.

# Running Locally
1. Make sure you have [Node.JS](https://nodejs.org/en/download) and [Docker](https://docs.docker.com/get-started/get-docker/) installed on your system. 
I would also recommend using [PNPM](https://pnpm.io/installation) as your package manager, instead of NPM (which comes with Node.JS by default).
1. Run `git clone https://github.com/oathompsonjones/A-Graphical-Playground-for-Haskell.git` to clone this repository.
1. Run `cd A-Graphical-Playground-for-Haskell` to navigate to the correct directory.
1. Set up a [MongoDB](https://www.mongodb.com) database.
1. Create a file called `.env` at the root of this directory, containing a variable called `MONGO_DB_URL`, containing the URL to your database.
1. Run `pnpm install` (or `npm install` if using NPM).
1. Run `pnpm build` (or `npm run build` if using NPM).
1. Run `pnpm start` (or `npm run start` if using NPM).

# Reporting Bugs
Please use the [issue tracker](https://github.com/oathompsonjones/A-Graphical-Playground-for-Haskell/issues) to report any bugs, and provide as much information as you can, including how to reproduce the bug where possible.