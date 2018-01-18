import * as express from 'express'
import {FizzBuzzControl} from './src/fizz-buzz-control'

const app = express()
const router = express.Router()
const baseUrl = process.env.npm_package_config_baseUrl
const port = process.env.npm_package_config_port

const message = (m: string) => ({message: m})
const control = new FizzBuzzControl()

router.get('/fizz-buzz/:limit', (request, response) => control
    .calculateUpTo(request.params.limit)
    .subscribe(
        value => response.json(value),
        (error: Error) => response.status(400).json({message: error.message, name: error.name})))

app.use(baseUrl, router)
app.use((request, response) => {
    response
        .status(404)
        .send(message('unknown command'))
})
console.log(`listening to port ${port}... (press Ctrl+C to cancel)`)
app.listen(port)
