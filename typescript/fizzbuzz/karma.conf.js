// Karma configuration
// Generated on Wed May 31 2017 00:01:22 GMT+0200 (CEST)

module.exports = function (config) {
    config.set({
        basePath: '',
        frameworks: ['jasmine', 'karma-typescript'],
        files: [
            'src/**/*.ts'
        ],
        exclude: [],
        preprocessors: {
            '**/*.ts': ['karma-typescript']
        },
        reporters: ['progress'],
        autoWatch: true,
        browsers: ['PhantomJS', 'Chrome'],
        singleRun: false,
        concurrency: Infinity
    })
};
