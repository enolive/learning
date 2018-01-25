export const getBaseUri = () => {
    return `http://localhost:${getPort()}`
}

export const getPort = () => {
    return require('../config/dev.json').port.toString()
}
