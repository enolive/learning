<script lang="ts">
  import {fizzBuzz} from './fizzbuzz';

  let input: number;
  let result: string;

  const calculateIt = () => {
    result = fizzBuzz(input);
  };
</script>

<main>
    <h1>Fizz Buzz</h1>
    <form on:submit|preventDefault={calculateIt}>
        <label>
            Enter a number:
            <input type="number" bind:value={input}>
        </label>
        <button>Calculate It</button>
        {#if result}
          <div role="status">Result: {result}</div>
        {/if}
    </form>
</main>

<style>
    main {
        text-align: center;
        padding: 1em;
        max-width: 240px;
        margin: 0 auto;
    }

    h1 {
        color: #ff3e00;
        text-transform: uppercase;
        font-size: 4em;
        font-weight: 100;
    }

    @media (min-width: 640px) {
        main {
            max-width: none;
        }
    }
</style>
