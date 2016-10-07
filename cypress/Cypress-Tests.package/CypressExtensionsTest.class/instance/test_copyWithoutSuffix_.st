"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
test_copyWithoutSuffix_

	self
		assert: ('abc' copyWithoutSuffix: 'c') equals: 'ab';
		assert: ('abc' copyWithoutSuffix: 'a') equals: 'abc';
		assert: ('abc' copyWithoutSuffix: 'abc') equals: '';
		assert: ('abc' copyWithoutSuffix: 'xxxabc') equals: 'abc';
		assert: ('abc' copyWithoutSuffix: '') equals: 'abc';
		assert: ('' copyWithoutSuffix: 'abc') equals: '';
		assert: ('' copyWithoutSuffix: '') equals: ''
