"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: Accessing - private
"
findPackagesEclipsedBy: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name
				and: [each name beginsWith: aKnownPackageInformation name , '-']]