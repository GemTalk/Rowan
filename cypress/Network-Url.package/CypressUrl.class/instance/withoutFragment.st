"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: fragment
"
withoutFragment
	"return a URL which is identical to the receiver except that it has no fragment associated with it"
	^self withFragment: nil
