tests
testUpdateUnpackagedClassComment

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