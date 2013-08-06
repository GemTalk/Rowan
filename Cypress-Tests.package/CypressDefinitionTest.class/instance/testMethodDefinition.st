"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
testMethodDefinition
	self assert: (CypressMethodDefinition
		className: 'Foo'
		classIsMeta: false
		selector: 'isFoo'
		category: 'testing'
		source: 'isFoo ^true') printString equals: 'a CypressMethodDefinition(Foo>>isFoo)'
