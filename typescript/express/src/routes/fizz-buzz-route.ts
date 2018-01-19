import * as express from 'express'
import {FizzBuzzControl} from '../services/fizz-buzz-control'

const router = express.Router()
const control = new FizzBuzzControl()

router.get('/numbers/:limit', (request, response) => control
    .calculateUpTo(request.params.limit)
    .subscribe(
        value => response.json(value),
        (error: Error) => response.status(400).json({message: error.message, name: error.name})))

export default router
