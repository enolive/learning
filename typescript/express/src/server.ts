import * as express from 'express'
import fizz_buzz_route from './routes/fizz-buzz-route'

const startServer = () => {
    const app = express()
    app.use('/api/fizz-buzz', fizz_buzz_route)
    app.use((request, response) => response
        .status(404)
        .send(message('unknown command')))

    const port = getPort()

    console.log(`listening to port ${port}... (press Ctrl+C to cancel)`)
    app.listen(port)

}
const getPort = () => {
    const value = +process.env.npm_package_config_PORT
    return isNaN(value) || !value
        ? 3000
        : value
}
const message = (m: string) => ({message: m})

startServer()
