tests
testAddUnpackagedMethod

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