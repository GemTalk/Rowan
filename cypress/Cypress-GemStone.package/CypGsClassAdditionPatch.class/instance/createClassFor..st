"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
createClassFor: aPatchSet

	newClass := self privateCreateClassFor: aPatchSet.
	symbolAssociation := aPatchSet createdClass: newClass.
	^newClass