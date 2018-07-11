module.exports = function (config) {
    config.set({
        basePath: 'lib',
        frameworks: ['jasmine'],
        files: [
            {pattern: 'src/**/*.js'},
            {pattern: 'test/**/*.js'}
        ],
        preprocessors: {
            'src/**/*.js': ['coverage', 'sourcemap'],
            'test/**/*.js': ['sourcemap']
        },
        reporters: ['progress', 'coverage', 'remap-coverage'],
        coverageReporter: {
            reporters: [
                {type: 'in-memory'}
            ]
        },
        remapCoverageReporter: {
            'text-summary': null,
            html: './coverage/html',
            cobertura: './coverage/cobertura.xml',
            lcovonly: './coverage/lcov.info'
        },
        browsers: ['ChromiumHeadless']
    });
};