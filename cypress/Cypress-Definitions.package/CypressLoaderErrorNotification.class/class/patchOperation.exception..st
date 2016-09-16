"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
patchOperation: aPatchOperation exception: anException

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself