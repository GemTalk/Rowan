"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
packagesNamed: someNames
	"Create a PackageSet by names only, resolved from known repositories"

	| packageRefs |
	packageRefs := someNames collect: [:aName | self packageNamed: aName].
	^CypPackageSet withAll: packageRefs