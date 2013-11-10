"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: Initializing - private
"
initializePackageInformationList

	self
		packageInformationList: (self potentialPackageNames
				inject: Dictionary new
				into: 
					[:dict :each |
					dict
						at: each put: (CypressUnknownPackageInformation named: each);
						yourself])