"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
test_parseSelectorFrom_

	self
		assert: (UndefinedObject parseSelectorFrom: 'a') equals: 'a';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a self halt') equals: 'a';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a: something') equals: 'a:';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a: something b: else') equals: 'a:b:';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: '= another ^false') equals: '=';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: '=@= another ^false') equals: '=@=';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';

		assert: (UndefinedObject parseSelectorFrom: 'a ^undefined') equals: 'a';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a: something undefined := something') equals: 'a:';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: 'a: something b: else ^something =@= else') equals: 'a:b:';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: '= another ^undefined = another') equals: '=';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind';
		assert: (UndefinedObject parseSelectorFrom: '=@= another ^undefined =@= another') equals: '=@=';
		deny: (UndefinedObject includesCategory: #'xyzzy') description: '#parseSelectorFrom: should not leave anything behind'
