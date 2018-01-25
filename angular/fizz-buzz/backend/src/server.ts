import * as express from 'express'
import fizz_buzz_route from './boundary/fizz-buzz-route'
import {getBaseUri, getPort} from './read-config'
import * as url from 'url'

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

    const result = url.parse(getBaseUri())
    const port = +result.port
    const host = result.hostname

    app.listen(port, host, () => {
        console.log(`listening on ${port} ${host}.. (press Ctrl+C to cancel)`)
    })

}

const message = (m: string) => ({message: m})

startServer()
