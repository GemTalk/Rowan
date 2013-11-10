"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testCypressUrls

	| url |
	#(
		( 'cypress:/a/b/c'			'Cypress'	true		'cypress:///a/b/c'			#('a' 'b' 'c') )
		( 'cypresslax:/a/b/c'		'Cypress'	false	'cypresslax:///a/b/c'		#('a' 'b' 'c') )
		( 'cypressft:/a/b/c'		'FileTree'	false	'cypressft:///a/b/c'		#('a' 'b' 'c') )
		( 'cypressfiletree:/a/b/c'	'FileTree'	true		'cypressfiletree:///a/b/c'	#('a' 'b' 'c') )
	) do: [:tuple |
		url := CypressAbstractFileUrl absoluteFromText: tuple first.
		self
			assert: url printString equals: (tuple at: 4);
			assert: url codeFormat equals: (tuple at: 2);
			assert: url isStrict equals: (tuple at: 3);
			assert: url path equals: (tuple at: 5);
			assert: url host equals: ''
	].
	url := CypressAbstractFileUrl absoluteFromText: '/a/b/c/'.
	self
		assert: url printString equals: 'file:///a/b/c/';
		assert: url path equals: #('a' 'b' 'c' '');
		assert: url host equals: ''.
	url := CypressAbstractFileUrl absoluteFromText: 'polution:/a/b/c/'.
	self
		assert: url printString equals: 'polution:/a/b/c/';
		assert: url schemeName equals: 'polution';
		assert: url locator equals: '/a/b/c/'
