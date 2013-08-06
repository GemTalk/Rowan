"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
test_escapePercents

	self
		assert: 'aa aa éé aa aa' encodeAsUTF8 escapePercents
			equals: 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa';
		assert: 'aa aa éé aa aa' escapePercents
			equals: 'aa%20aa%20%E9%E9%20aa%20aa'