"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
createClassFor: aPatchSet

	| createdClass |
	createdClass := self privateCreateClassFor: aPatchSet.
	aPatchSet createdClass: createdClass.
	^createdClass