"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
createClassFor: aPatchSet

	newClass := super createClassFor: aPatchSet.
	symbolAssociation := aPatchSet associationFor: newClass name.
	^newClass