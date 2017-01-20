"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
initializeForName: aName

	super initializeForName: aName.
	properties at: 'name' put: aName.
	PackageRegistry at: aName put: self