tests
test_copyWithoutSuffix_or_

	self
		assert: ('abc' copyWithoutSuffix: 'c' or: [nil]) equals: 'ab';
		assert: ('abc' copyWithoutSuffix: 'a' or: [nil]) equals: nil;
		assert: ('abc' copyWithoutSuffix: 'abc' or: [nil]) equals: '';
		assert: ('abc' copyWithoutSuffix: 'xxxabc' or: [nil]) equals: nil;
		assert: ('abc' copyWithoutSuffix: '' or: [nil]) equals: nil;
		assert: ('' copyWithoutSuffix: 'abc' or: [nil]) equals: nil;
		assert: ('' copyWithoutSuffix: '' or: [nil]) equals: nil
