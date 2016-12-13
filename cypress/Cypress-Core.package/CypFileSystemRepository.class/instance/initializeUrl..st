"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing
"
initializeUrl: anUrl

	super initializeUrl: anUrl.
	disk := Cypress fileUtilities.
	self initializeRepositoryDirectory.
	self readPropertiesFile