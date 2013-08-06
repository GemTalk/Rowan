"
notice: This work is protected by copyright. All rights reserved.
category: instance creation
"
fromJs: jsObject

	^(self new) 
		fromJs: jsObject asCypressPropertyObject;
		yourself
