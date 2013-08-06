"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
test_withUnixLineEndings

	| lf cr crlf |
	lf := String with: Character lf.
	cr := String with: Character cr.
	crlf := cr, lf.

	self
		assert: (self unixLinesFrom: '') equals: #();
		assert: (self unixLinesFrom: 'abc') equals: #('abc');
		assert: (self unixLinesFrom: 'abc', lf) equals: #('abc');
		assert: (self unixLinesFrom: 'abc', cr) equals: #('abc');
		assert: (self unixLinesFrom: 'abc', crlf) equals: #('abc');
		assert: (self unixLinesFrom: 'abc', lf, cr) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', cr, cr) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', crlf, cr) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', lf, lf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', crlf, lf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', lf, crlf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', cr, crlf) equals: #('abc' '');
		assert: (self unixLinesFrom: 'abc', crlf, crlf) equals: #('abc' '')