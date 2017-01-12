"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
initializeForName: aClassName

	super initializeForName: aClassName.
	properties at: 'name' put: aClassName.
	ClassRegistry at: aClassName put: self