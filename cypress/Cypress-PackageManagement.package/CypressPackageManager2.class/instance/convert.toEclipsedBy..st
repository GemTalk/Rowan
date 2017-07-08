"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Updating
"
convert: anUnknownPackageInformation toEclipsedBy: aKnownPackageInformation

	| eclipsed |
	eclipsed := CypressEclipsedPackageInformation
				fromUnknown: anUnknownPackageInformation
				eclipsedBy: aKnownPackageInformation.
	self replace: anUnknownPackageInformation with: eclipsed.
	^eclipsed
