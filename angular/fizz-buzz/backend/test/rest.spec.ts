import chai = require('chai')
import chaiHttp = require('chai-http')

chai.use(chaiHttp)

import {expect} from 'chai'
import {getBaseUri, getPort} from '../src/read-config'

describe('REST API for Fizz-Buzz', () => {
    it('should have an HTTP configuration', () => {
        expect(getPort()).not.be.empty
        expect(getBaseUri()).not.be.empty
    })

    it('should work', done => {
        chai.request(getBaseUri())
            .get('/api/fizz-buzz/numbers/15')
            .end((err, res) => {
                expect(res).to.have.status(200).and.to.be.json
                expect(res.body).to.deep.equal([
                    '1',
                    '2',
                    'Fizz',
                    '4',
                    'Buzz',
                    'Fizz',
                    '7',
                    '8',
                    'Fizz',
                    'Buzz',
                    '11',
                    'Fizz',
                    '13',
                    '14',
                    'Fizz-Buzz',
                ])
                done()
            })
    })

    describe('range errors', () => {
        [0, 1001].forEach(limit =>
            it(`should fail on limit ${limit}`, done => {
                chai.request(getBaseUri())
                    .get(`/api/fizz-buzz/numbers/${limit}`)
                    .end(err => {
                        expect(err).to.have.status(400)
                        done()
                    })
            }))
    })

    describe('not existing resources', () => {
        [
            '/',
            '/api/fizz-buzz',
            '/api/fizz-buzz/numbers',
            '/api/fizz-buzz/numbers/',
        ].forEach(uri =>
            it(`should fail on invalid URI ${uri}`, done => {
                chai.request(getBaseUri())
                    .get(uri)
                    .end(err => {
                        expect(err, `failed on URI ${uri}`).to.have.status(404)
                        done()
                    })
            }))
    })
})
