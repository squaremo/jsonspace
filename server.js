var REJSON = require('rejson');

sockjs = require('sockjs').createServer({prefix: '/query'}),
http = require('http').createServer();

sockjs.installHandlers(http);

sockjs.on('connection', hookup_interpreter);

function hookup_interpreter(connection) {
  // connection.setEncoding('utf8');
  connection.on('data', function(data) {
    console.log(data);
    if (data.substr(0, 6) === 'WRITE ') {
      var value = JSON.parse(data.substr(6));
      write_value(value);
    }
    else {
      var query = REJSON.parse(data.substr(5));
      if (data.substr(0, 5) === 'READ ') {
        find_value(query, connection, true);
      }
      else if (data.substr(0, 5) === 'TAKE ') {
        find_value(query, connection, false);
      }
    }
  });
}

// both queues
var VALUES = [];
var OUTSTANDING = [];

function try_outstanding(value) {
  for (var i=0, len = OUTSTANDING.length; i < len; i++) {
    var entry = OUTSTANDING[i];
    if (entry.query.test(value)) {
      OUTSTANDING.splice(i);
      if (entry.replace) {
        OUTSTANDING.push(entry);
      }
      entry.connection.write(JSON.stringify(value));
      return true;
    }
  }
  return false;
}

function write_value(value) {
  if (!try_outstanding(value)) {
    VALUES.push(value);
  }
}

function find_value(query, connection, replace) {
  for (var i=0, len = VALUES.length; i < len; i++) {
    var value = VALUES[i];
    if (query.test(value)) {
      connection.write(JSON.stringify(value));
      VALUES.splice(i);
      if (replace) {
        VALUES.push(value);
      }
      return true;
    }
  }
  OUTSTANDING.push({query: query, connection: connection, replace: replace});
  return false;
}

http.listen(5000);
