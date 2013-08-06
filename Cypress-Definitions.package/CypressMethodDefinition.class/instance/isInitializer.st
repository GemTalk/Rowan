"
notice: This work is protected by copyright. All rights reserved.
category: testing
"
isInitializer
	^ self selector = 'initialize' and: [self classIsMeta]
