<script lang="ts">
  interface JokeText {
    value: string
  }

  const fetchRandomJoke = (): Promise<string> => fetch('https://api.chucknorris.io/jokes/random')
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
      <div class="spinner-border">
        <span class="visually-hidden">Loading...</span>
      </div>
    {:then jokeText}
      <div class="alert alert-info">Random wisdom: {jokeText}</div>
    {:catch error}
      <div class="alert alert-danger">{error}</div>
    {/await}
  </p>
  <button on:click={() => fetchJoke$ = fetchRandomJoke()} class="btn btn-primary">Refresh</button>
</main>
