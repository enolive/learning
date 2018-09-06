import * as R from "ramda";
import seedrandom from "seedrandom";

export function beerify(songText, frequency, rng = seedrandom) {
    const words = takeWords(songText);
    const weights = weight(words, randomness(rng));
    return replaceWithBeer(songText, weights, frequency);
}

export function takeWords(sentence) {
    const notEmpty = sentence => sentence.length > 0;
    return R.compose(
        R.uniq,
        R.filter(notEmpty),
        R.split(/\W/))(sentence);
}

export function weight(words, fn) {
    return R.map(word => [word, fn()])(words);
}

export function randomness(rng = seedrandom()) {
    return () => Math.round(rng.double() * 100);
}

export function replaceWithBeer(sentence, weights, frequency) {
    const replaceSingle = (acc, [word]) => R.replace(allOccurrencesOf(word), '🍺', acc);
    const allOccurrencesOf = word => new RegExp(`\\b${word}\\b`, 'g');
    const weightDesc = ([word]) => -word.length;
    const underFrequency = ([_, val]) => frequency > 0 && val <= frequency;
    return R.compose(
        R.reduce(replaceSingle, sentence),
        R.sortBy(weightDesc),
        R.filter(underFrequency),
    )(weights);
}