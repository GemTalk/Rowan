tests
test_indexOfAnyOf_startingAt_ifAbsent_

	self
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 1 ifAbsent: [nil]) equals: 5;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 6 ifAbsent: [nil]) equals: 6;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 7 ifAbsent: [nil]) equals: 14;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 15 ifAbsent: [nil]) equals: 15;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 16 ifAbsent: [nil]) equals: 20;
		assert: ('word, another, done.' indexOfAnyOf: '., ' startingAt: 21 ifAbsent: [nil]) equals: nil;
		assert: ('word, another, done.' indexOfAnyOf: '+-' startingAt: 1 ifAbsent: [nil]) equals: nil
