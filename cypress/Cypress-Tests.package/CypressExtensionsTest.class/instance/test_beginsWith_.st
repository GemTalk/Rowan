"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
test_beginsWith_

	self
		assert: ('abc' beginsWith: 'a');
		deny: ('abc' beginsWith: 'c');
		assert: ('abc' beginsWith: 'abc');
		deny: ('abc' beginsWith: 'abcx');
		deny: ('abc' beginsWith: '');
		deny: ('' beginsWith: 'abc');
		deny: ('' beginsWith: '')
