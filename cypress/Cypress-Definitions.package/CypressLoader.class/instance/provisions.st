"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
provisions
	^ provisions ifNil: [provisions := (self allClasses collect: [:cl | cl name asString]) asSet ]
