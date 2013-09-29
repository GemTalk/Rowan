"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: testing properties
"
isCodeFormatFlexiblyFileTree

	^self isCodeFormatStrict not and: [self isCodeFormatFileTree]
