tests-issue 72
testIssue72_updateMethod
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
			"attempt to update existing unpackaged method - expect error"
			Object
				rwCompileMethod: 'bar ^''foo'''
				category: 'accessing' ]
		raise: Error.
	self assert: Object new bar = 'bar'.

	[
		"handle notification and abort the operation"
		Object
			rwCompileMethod: 'bar ^''foo'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | "abort"].
	self assert: Object new bar = 'bar'.

	[
		"handle notification and complete operation"
		Object
			rwCompileMethod: 'bar ^''foo'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self assert: Object new bar = 'foo'.

	Object removeSelector: #'bar'
