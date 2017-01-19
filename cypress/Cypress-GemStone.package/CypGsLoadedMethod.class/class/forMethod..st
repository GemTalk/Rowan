"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
forMethod: aGsNMethod

	| instance |
	instance := self newNamed: aGsNMethod selector asString.
	instance
		handle: aGsNMethod;
		registerYourself.
	^instance