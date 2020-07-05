// Imports
const sh = require('shelljs');


// Project
const root = sh.pwd().toString();
const ghpages = `${root}/gh-pages`;


// Target
const target = `${root}/target`;
const appTarget = `${target}/app`;


// Run the Production build
sh.exec('npm run build:prod');

// Clean gh-pages
sh.rm('-rf', `${ghpages}/*`);


// Copy APP to gh-pages
sh.cp('-rf', `${appTarget}/*`, `${ghpages}/`);


// Publish!
sh.cd(ghpages);
sh.exec('git add .');
sh.exec('git commit -m "publish latest changes"');
sh.exec('git push');
