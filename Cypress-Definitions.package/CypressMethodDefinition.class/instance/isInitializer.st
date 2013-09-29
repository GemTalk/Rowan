"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
isInitializer
	^ self selector = 'initialize' and: [self classIsMeta]
