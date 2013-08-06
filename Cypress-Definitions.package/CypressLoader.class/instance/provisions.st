"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
provisions
	^ provisions ifNil: [provisions := (self allClasses collect: [:cl | cl name asString]) asSet ]
