"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
test_endsWith_

	self
		assert: ('abc' endsWith: 'c');
		deny: ('abc' endsWith: 'a');
		assert: ('abc' endsWith: 'abc');
		deny: ('abc' endsWith: 'xabc');
		deny: ('abc' endsWith: '');
		deny: ('' endsWith: 'abc');
		deny: ('' endsWith: '')
