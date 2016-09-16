"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing
"
initializeFromName: aString andRepository: aCypressRepositoryOrNil

	self name: aString.
	aCypressRepositoryOrNil isNil ifTrue: [^self].
	self updateKnownPackageRepository: aCypressRepositoryOrNil