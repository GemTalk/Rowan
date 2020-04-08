Class {
	#name : 'RwUnpackagedBrowserApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwUnpackagedBrowserApiTest >> _testClass [

	^ Rowan globalNamed: self _testClassName
]

{ #category : 'private' }
RwUnpackagedBrowserApiTest >> _testClassName [

	^  'Issue364_class'
]

{ #category : 'private' }
RwUnpackagedBrowserApiTest >> _testPackagedClassName [

	^  'Issue364_class_packaged'
]

{ #category : 'running' }
RwUnpackagedBrowserApiTest >> setUp [
	"test class will be removed along with symbol dictionary during teardown"

	| symDict |
	super setUp.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	Object subclass: self _testClassName
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #()
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testAddUnpackagedClassMethod [

	"https://github.com/GemTalk/Rowan/issues/547"

	"Add unpackaged method to an upackaged class"

	| testClass audit |

	testClass := self _testClass.
	[ testClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'issue 364' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass class compiledMethodAt: 'bar' otherwise: nil) notNil.

	[ testClass class rwRemoveSelector: #bar ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testAddUnpackagedMethod [

	"https://github.com/GemTalk/Rowan/issues/364"

	"Add unpackaged method to an upackaged class"

	| testClass audit |

	testClass := self _testClass.
	[ testClass
		rwCompileMethod: 'bar ^''bar'''
		category: 'issue 364' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.

	[ testClass rwRemoveSelector: #bar ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testMoveUnpackagedMethod [

	"https://github.com/GemTalk/Rowan/issues/364"

	"Add unpackaged method"

	| audit x testClass |

	testClass := self _testClass.
	[ testClass
		rwCompileMethod: 'bar ^''bar'''
		category: 'issue 364' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (x := testClass categoryOfSelector: #bar) = #'issue 364'.

	testClass addCategory: 'issue 364 other'.
	testClass rwMoveMethod: #bar toCategory: 'issue 364 other'.	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (x := testClass categoryOfSelector: #bar) = #'issue 364 other'.

	[ testClass rwRemoveSelector: #bar  ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testUpdateUnpackagedClassComment [

	"https://github.com/GemTalk/Rowan/issues/364"

	"Add comment to unpackaged class"

	| audit testClass |

	testClass := self _testClass.
	[ testClass
		rwComment: 'a comment' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: testClass comment = 'a comment'.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]
