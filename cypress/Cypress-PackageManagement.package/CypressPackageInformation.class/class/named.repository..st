"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
named: aString repository: aCypressRepository
	"Answer an instance of the receiver representing the named package.
	 If the package was saved in a Repository, load up the saved details."

	^self new
		initializeFromName: aString andRepository: aCypressRepository;
		yourself
