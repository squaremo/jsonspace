function JsonSpace(url) {
    this._connection = new SockJS(url);
}

// Use refs.

JsonSpace.prototype = {
    read: function(pattern, k) {
        this._connection.onmessage = function(msg) {
            k(JSON.parse(msg.data));
        };
        this._connection.send('read ' + toReJsonPattern(pattern));
    },

    write: function(value) {
        this._connection.send('write ' + JSON.stringify(value));
    },

    take: function(pattern, k) {
        this._connection.onmessage = function(msg) {
            k(JSON.parse(msg.data));
        };
        this._connection.send('take ' + toReJsonPattern(pattern));
    }
};

// Pattern constructors

/* EG

// maybe doesn't really work in the matcher ..
space.read([1, 2, $number.maybe()],
           function(val) {
               document.getElementById('foo').value = val;
           });

// This looks ok. Shame about the extra parens. NB would have to
// examine each value to see whether it was a pattern.
space.take(({foo: $string}).etc(), function(val) {...});

// Easy.
space.write({foo: 1, bar: 2});

// 
space.read(({foo: $number.as('foo')}).as('obj'),
           function(bindings) { use(bindings.foo); ...});



*/

function $_(something) {
    if (something['toReJsonPattern']) {
        return something;
    }
    //else
    switch (typeof(something)) {
    case 'object':
        return (something instanceof Array) ?
            new $_array(something):
            new $_object(something);
    case 'string':
    case 'number':
    case 'boolean':
        return new $_literal(something);
    }
}

function toReJsonPattern(something) {
    return $_(something).toReJsonPattern();
}

function $_pattern() {
}
$_pattern.prototype = {
    'merge': function(obj) {
        for (var k in obj) {
            if (obj.hasOwnProperty(k)) {
                this[k] = obj[k];
            }
        }
        return this;
    },

    // Assign to a variable
    'as': function(name) {
        return new $_binding(name, this);
    },
    // Combinators
    'or': function(something) {
        return new $_binary('|', this, something);
    },
    // Qualifiers. Annoyingly, we cannot restrict these syntactically.
    'maybe': function() {
        return new $_postfix('?', this);
    },
    'maybeRepeated': function() {
        return new $_postfix('*', this);
    },
    'repeated': function() {
        return new $_postfix('+', this);
    }
};

function $_binary(combinator, lhs, rhs) {
    this._combinator = combinator;
    this._lhs = lhs;
    this._rhs = rhs;
}
$_binary.prototype = new $_pattern().merge({
    'toReJsonPattern': function() {
        return toReJsonPattern(this._lhs) + this._combinator +
            toReJsonPattern(this._rhs);
    }
});

function $_postfix(operator, arg) {
    this._operator = operator;
    this._arg = arg;
}
$_postfix.prototype = new $_pattern().merge({
    toReJsonPattern: function() {
        return this._arg.toReJsonPattern() + ' ' + this._operator;
    }
});

function $_binding(name, pattern) {
    this._name = name;
    this._pattern = pattern;
};
$_binding.prototype = new $_pattern().merge({
    toReJsonPattern: function() {
        return this._name + ' = ' + this._pattern.toReJsonPattern();
    }
});

function $_literal(something) {
    this._value = something;
}
$_literal.prototype = new $_pattern().merge({
    toReJsonPattern: function() {
        return JSON.stringify(this._value);
    }
});

function $_object(obj, etc) {
    this._obj = obj;
    this._etc = etc;
}
$_object.prototype = new $_pattern().merge({
    toReJsonPattern: function() {
        var entries = [];
        var obj = this._obj;
        for (k in obj) {
            if (obj.hasOwnProperty(k)) {
                entries.push('"' + k + '": ' + toReJsonPattern(obj[k]));
            }
        }
        if (this._etc) {
            entries.push('_');
        }
        return '{' + entries.join(', ') + '}';        
    },
    etc: function() {
        return (this._etc) ? this : new $_object(this._obj, true);
    }
});

function $_array(something, etc) {
    this._arr = something;
    this._etc = etc;
}
$_array.prototype = new $_pattern().merge({
    toReJsonPattern: function() {
        var entries = [];
        var arr = this._arr;
        for (k in arr) {
            entries.push(toReJsonPattern(arr[k]));
        }
        if (this._etc) {
            entries.push('_ *');
        }
        return '[' + entries.join(', ') + ']';
    },
    etc: function() {
        return (this._etc) ? this : new $_array(this._arr, true);
    },
    interleave: function(that) {
        return new $_binary('^', this, that);
    }
});

function $_type(symbol) {
    this._sym = symbol;
}
$_type.prototype = new $_pattern().merge({
    toReJsonPattern: function() {
        return this._sym;
    }
});

var $any = new $_type('_');
var $number = new $_type('number');
var $string = new $_type('string');
