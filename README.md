
# morpheus-servant-persistent

## Dev Setup
Install the Haskell dev environment using ghcup:https://www.haskell.org/ghcup/install/ 

Configure your editor for Haskell. VSCode has a good plugin called 'Haskell for Visual Studio Code.' Installation instructions at https://github.com/haskell/vscode-haskell

You will need a working Postgres server running on your machine.

## Environment Variables

- SERVICE_PORT - default is 5051
- POSTGRES - postgres connection string; default is "host=localhost port=5432 user=postgres dbname=postgres password=postgres"
- ENVIRONMENT - default is development

Run `source .env` to load the defaults from .env

## Commands

*This project uses the `stack` build tool which is installed by default during installation of `ghcup`. As of November 2023 the Morpheus GraphQL library does not play well with the `cabal` build tool.*

`stack run config` attempts to read the environment variables needed to run the server and display them. If there is an error it displays the default config values found in `src/Config.hs`

`stack run db show` prints the SQL migrations that need to be run to make the postgres database match the schema defined in `src/DBSchema.hs`

`stack run db migrate` executes the migrations needed to update the postgres database to match the schema defined in `src/DBSchema.hs`

`stack run` starts the server

Point your browser to http://localhost:5051/status to verify that the servier is running

Navigate to http://localhost:5051/schema to access the graphql playground. *Note that the graphql api is served from http://localhost:5051/graphql, so you will need to use that url in the graphql playground's address bar.*

## Sample GraphQL Payloads

    mutation {
        createPatient(name:"Somebody", email:"somebody@evernow.com", age:99, favoriteColor:"blue", birthdate:"1975-05-25T22:06:04.183000Z"){
            name,
            email,
            age,
            favoriteColor
        }
    }

.

    mutation {
        reportSymptom(description:"Headaches", patientId: 2, severity:5){
            description,
            id,
            patientId,
            severity,
            reportedAt
        }
    }

.

    query {
        patient(id:2){
            name,
            email,
            age,
            favoriteColor,
            birthdate
        }
    }

.

    query {
        patientsByFavoriteColor(favoriteColor:"blue"){
            id,
            name,
            age,
            email,
            favoriteColor,
            birthdate
        }
    }

.

    query {
        patientsBySymptom(description:"Headaches"){
            id,
            name,
            age,
            email,
            favoriteColor,
            birthdate
        }
    }