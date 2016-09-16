"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Updating
"
addUnknownPackageNamed: aString

	self packageInformationList
		at: aString
		put: (CypressUnknownPackageInformation named: aString).
