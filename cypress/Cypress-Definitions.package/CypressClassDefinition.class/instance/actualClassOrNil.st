"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
actualClassOrNil

	^self resolveGlobalNamed: self name or: [nil]
