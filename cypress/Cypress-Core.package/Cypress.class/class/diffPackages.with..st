"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
diffPackages: beforePackages with: afterPackages
	"Compare two PackageSets, answering a PackageSetModification"

	| beforeDefinitions afterDefinitions |
	beforeDefinitions := beforePackages definitions.
	afterDefinitions := afterPackages definitions.
	^afterDefinitions compareAgainstBase: beforeDefinitions