tests-issue 72
testIssue72_removeMethod
	"https://github.com/dalehenrich/Rowan/issues/72"

	[
		"add unpackaged method"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self assert: Object new bar = 'bar'.

	self 
		should: [
			"attempt to remove existing unpackaged method - expect error"
			Object rwRemoveSelector: #bar ]
		raise: Error.
	self assert: Object new bar = 'bar'.

	[
		"handle notification and abort the remove"
		Object rwRemoveSelector: #bar ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | "abort"].
	self assert: Object new bar = 'bar'.

	[
		"handle notification and complete removal"
		Object rwRemoveSelector: #bar  ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self should: [ Object new bar ] raise: MessageNotUnderstood.
