# aws-lambda

Just a simple test for deploying Haskell Functions to AWS lambda

## Prerequisites

* [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
* [Docker](https://www.docker.com)
* make tools (e.g. build-essentials)
* [Serverless Framework](https://www.serverless.com/)

## Build

Just run `make` in the project root. It will create a custom AWS runtime and put it zipped inside the build folder

## Deploy

Just run `sls deploy`.
