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
        reporters: ['progress', 'coverage', 'karma-remap-istanbul'],
        remapIstanbulReporter: {
            reports: {
                html: 'coverage',
                lcovonly: './coverage/coverage.lcov'
            }
        },
        browsers: ['ChromiumHeadless']
    });
};