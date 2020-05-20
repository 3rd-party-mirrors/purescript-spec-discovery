"use strict";

if (typeof require !== "function") {
  throw new Error(
    "Sorry, purescript-spec-discovery only supports NodeJS environments!"
  );
}

var fs = require("fs");
var path = require("path");

function getMatchingModules(pattern, specProperty) {
  var directories = fs.readdirSync(path.join(__dirname, ".."));
  var regexExp = new RegExp(pattern);
  return directories
    .filter(function (directory) {
      return regexExp.test(directory);
    })
    .map(function (name) {
      var module = require(path.join(__dirname, "..", name));
      return module && typeof module[specProperty] !== "undefined"
        ? module[specProperty]
        : null;
    })
    .filter(function (x) {
      return x;
    });
}

exports.getSpecs = function(pattern, property){
  return getMatchingModules(pattern, property);
};
