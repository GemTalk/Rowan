"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
newNamed: aName

	^(self new)
		initialize;
		initializeForName: aName;
		yourself