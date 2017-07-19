const yaml = require("js-yaml");

function decodeYamlImpl__(left) {
  return function decodeYamlImpl_(right) {
    return function decodeYamlImpl(value) {
      try {
        return right(yaml.safeLoad(value));
      } catch(e) {
        return left(e);
      }
    }
  }
}

function encodeYamlImpl(value) {
  return yaml.safeDump(value);
}

exports.decodeYamlImpl = decodeYamlImpl__;
exports.encodeYamlImpl = encodeYamlImpl;
