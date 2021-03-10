<script lang="ts">
  interface JokeText {
    value: string;
  }

  const fetchRandomJoke = () => fetch('https://api.chucknorris.io/jokes/random')
    .then(res => res.json() as JokeText)
    .then(joke => joke.value);

  let fetchJoke$ = fetchRandomJoke();
</script>

<header>
  <h1>Chuck Norris</h1>
</header>
<main>
  <figure>
    <img src="chuck-norris-logo.png" alt="Chuck Norris approves!">
    <figcaption>Chuck Norris approves!</figcaption>
  </figure>
  <p role="status">
    {#await fetchJoke$}
      Loading...
    {:then jokeText}
      Random Wisdom: {jokeText}
    {/await}
  </p>
  <button on:click={() => fetchJoke$ = fetchRandomJoke()}>Refresh</button>
</main>
