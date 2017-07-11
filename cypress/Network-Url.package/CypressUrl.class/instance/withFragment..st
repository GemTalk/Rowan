"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: fragment
"
withFragment: newFragment
	"return a URL which is the same except that it has a different fragment"
	^self copy privateFragment: newFragment; yourself
