"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
initializeForClass: aClass
	self initialize.
	name := aClass name asString.
	handle := aClass