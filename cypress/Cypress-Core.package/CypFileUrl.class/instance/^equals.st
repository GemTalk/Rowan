"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
= aFileUrl

	^self class = aFileUrl class & (host = aFileUrl host)
		& (fragment = aFileUrl fragment) & (isAbsolute = aFileUrl isAbsolute)
		& (path = aFileUrl path)