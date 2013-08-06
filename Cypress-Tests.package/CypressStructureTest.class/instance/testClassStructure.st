"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
testClassStructure

	| jsObject packageStructure classStructure classProperties |
	jsObject := self compileJSON: self basePackageStructureJson.
	packageStructure := CypressPackageStructure fromJs: jsObject.
	classStructure := packageStructure classes first.
	self assert: classStructure name equals: 'CypressMockBasic'.
	self deny: classStructure isClassExtension description: 'Class structure should not have been an extension'.
	self assert: classStructure comment equals: 'This mock contains basic class and instance method selectors'.
	classProperties := classStructure properties.
	self assert: classProperties size equals: 6.
	self assert: (classProperties at: 'instvars') equals: #('name').
	self assert: (classProperties at: 'classinstvars') equals: #('current').
	self assert: (classProperties at: 'name') equals: 'CypressMockBasic'.
	self assert: (classProperties at: 'super') equals: 'Object'.
	self assert: classStructure instanceMethods size equals: 4.
	self assert: classStructure classMethods size equals: 3.
	classStructure := packageStructure extensions first.
	self assert: classStructure name equals: 'Object'.
	self assert: classStructure isClassExtension description: 'Class structure should have been an extension'.
	self assert: classStructure comment equals: ''.
	classProperties := classStructure properties.
	self assert: classProperties size equals: 1.
	self assert: (classProperties at: 'name') equals: 'Object'.
	self assert: classStructure instanceMethods size equals: 1.
	self assert: classStructure classMethods size equals: 0.
