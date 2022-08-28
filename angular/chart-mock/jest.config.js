module.exports = {
  preset: 'jest-preset-angular',
  globalSetup: 'jest-preset-angular/global-setup',
  setupFilesAfterEnv: [
    '<rootDir>/setup-jest.ts'
  ],
  moduleNameMapper: {
    "^lodash-es$": "lodash"
  },
}
