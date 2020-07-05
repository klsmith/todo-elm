// Imports
const sh = require('shelljs');


// Project
const root = sh.pwd().toString();
const node_modules = `${root}/node_modules`;


// Source
const src = `${root}/src`;
const elmSrc = `${src}/elm`;
const webSrc = `${src}/web`;


// Target
const target = `${root}/target`;
const tempTarget = `${target}/temp`;
const appTarget = `${target}/app`;


// Create TEMP and APP if they don't exist
sh.mkdir('-p', tempTarget);
sh.mkdir('-p', appTarget);

// Clean the TEMP and APP directories
sh.rm('-rf', `${tempTarget}/*`);
sh.rm('-rf', `${appTarget}/*`);

// Build web to TEMP
sh.cp('-rf', `${webSrc}/*`, `${root}/package*.json`, tempTarget);
sh.cd(tempTarget);
sh.exec(`npm install --production`);
sh.rm('-f', 'package*.json');
sh.cd(root);


// Build elm to TEMP
sh.exec(`elm make ${elmSrc}/Main.elm --output=${tempTarget}/elm.js --optimize`);


// Copy TEMP to APP
sh.mv('-f', `${tempTarget}/*`, `${appTarget}/`);


// Delete TEMP
sh.rm('-rf', tempTarget);
