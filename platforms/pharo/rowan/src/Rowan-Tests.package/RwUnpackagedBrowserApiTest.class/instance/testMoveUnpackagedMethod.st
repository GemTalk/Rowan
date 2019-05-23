tests
testMoveUnpackagedMethod

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