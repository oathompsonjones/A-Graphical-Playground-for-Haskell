# next-init
This repository contains the boilerplate for a Next.js TypeScript project.
To set your project up with just one command, add the following script to your `.bashrc` file:
```sh
function nextinit() {
    # Clones the repo
    git clone --depth=1 https://github.com/oathompsonjones/next-init.git .
    # Remove the git history
    rm -rf ./.git
    # Reinitialises git
    git init
    # Installs the dependencies
    pnpm i
    pnpm update --latest
    # Creates files which git ignores
    touch .env
    mkdir .vscode
    touch .vscode/settings.json
    echo -e "{\n\t\"vitest.commandLine\": \"pnpx vitest watch\",\n\t\"vitest.enable\": true\n}" > .vscode/settings.json
    mkdir public
    mkdir src/images
    mkdir src/hooks
    mkdir src/contexts
    mkdir src/assets
    mkdir src/app/api
    mkdir src/app/\(redirects\)
    # Opens the index file in VSCode
    code ./src/app/\(pages\)/\(home\)/page.tsx
}
```
To use the function, simply run the command `nextinit` in an empty directory using a fresh bash terminal.
