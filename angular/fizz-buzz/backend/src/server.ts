import * as express from 'express'
import fizz_buzz_route from './boundary/fizz-buzz-route'

const startServer = () => {
    const app = express()
    app.all('/*', (request, response, next) => {
        const originFromRequest = request.headers.origin as string || ''
        if (originFromRequest.startsWith('http://localhost:')) {
            response.header('Access-Control-Allow-Origin', originFromRequest)
        }
        next()
    })
    app.use('/api/fizz-buzz', fizz_buzz_route)
    app.use((request, response) => {
        return response
            .status(404)
            .send(message('unknown command'))
    })

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
