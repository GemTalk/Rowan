"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
= aPatchOperation
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]
