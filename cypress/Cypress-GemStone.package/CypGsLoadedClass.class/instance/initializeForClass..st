"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
initializeForClass: aClass

	self initialize.
	self initializeForName: aClass name asString.
	handle := aClass.
	ClassRegistry at: aClass classHistory put: self