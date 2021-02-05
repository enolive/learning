# Fizz-Buzz with Svelte

using the minified power of [Svelte](http://svelte.dev) to test-drive a simple app.

I am using a combined setup of Typescript,Jest and Testing Library to test all aspects of the app. Might be a good starting point for any complex use case.

I feel that Svelte might be the tool of choice for frontend work where React is still too "enterprisy" 😜.

## Learnings

* I like the pragmatism of Svelte
    * no Hooks, Effects et al. required (just define some varibles in the script section)
    * everything in a single file
    * simple templating syntax
    * No HTML-ish DSL like React's JSX
* What I dislike
    * no first class testing support
    * default scaffolding does not include any test dependencies
    * have to add jest, svelte-jest, testing-library by myself
    * mutability for state variables (instead of state/reducers) as in React

## How to start

E`npm run dev` will run a dev server with the power of hot reloading.

`npm run test` will run all tests included in the project.

`npm run build` followed by `npm start` will start a production ready version of the app you can upload and server on an edge or wherever you wish.
