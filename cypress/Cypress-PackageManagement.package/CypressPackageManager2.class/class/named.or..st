"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Accessing
"
named: aKey or: aBlock
	"Answer the Package Manager previously saved under aKey.
	 Answer the result of evaluating aBlock, if there was not one saved under that key."

	^self savedPackageManagers at: aKey ifAbsent: aBlock