"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
registerYourself

	(MethodRegistry includesKey: handle)
		ifTrue: [self error: 'Method already registered.'].
	MethodRegistry at: handle put: self