function jsonToSchema(obj, localObj = { title: 'Configuration' }) {
  localObj.type = typeof obj;
  if (Array.isArray(obj)) localObj.type = 'array';

  localObj.default = obj;

  if (localObj.type === 'object') {
    delete localObj.default;
    const entries = Object.entries(obj);
    if (entries.length) localObj.properties = {};
    entries.forEach(([key, value]) => {
      localObj.properties[key] = jsonToSchema(value, { title: key });
    });
  }

  if (localObj.type === 'boolean') {
    localObj.format = 'checkbox';
  }

  if (localObj.type === 'array') {
    delete localObj.default;
    if (!obj.length) {
      localObj.items = { type: 'string' };
      return localObj;
    }
    localObj.items = {};
    localObj.uniqueItems = true;
    if (typeof obj[0] !== 'object') {
      localObj.default = obj[0];
      localObj.items.enum = obj;
    } else jsonToSchema(obj[0], localObj.items);
  }

  return localObj;
}
