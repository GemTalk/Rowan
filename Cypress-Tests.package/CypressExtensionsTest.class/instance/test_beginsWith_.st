tests
test_beginsWith_

	self
		assert: ('abc' beginsWith: 'a');
		deny: ('abc' beginsWith: 'c');
		assert: ('abc' beginsWith: 'abc');
		deny: ('abc' beginsWith: 'abcxxx');
		deny: ('abc' beginsWith: '');
		deny: ('' beginsWith: 'abc');
		deny: ('' beginsWith: '')
