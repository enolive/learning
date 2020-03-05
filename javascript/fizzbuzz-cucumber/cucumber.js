let common = [
  'features/**/*.feature',
  '--require-module ts-node/register',
  '--require step-definitions/**/*.ts',
].join(' ');

module.exports = {
  default: common
};
