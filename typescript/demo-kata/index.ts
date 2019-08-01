import * as R from 'ramda';
import {mkRainDrops} from './src/raindrops';

const print = (line: string) => console.log(line);

R.pipe(
    R.range(1),
    R.map(mkRainDrops),
    R.forEach(print),
)(100);
