"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
test_indexOfAnyOf_startingAt_

	self
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1) equals: 5;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6) equals: 6;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7) equals: 14;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15) equals: 15;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16) equals: 20;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21) equals: 0;
		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1) equals: 0
