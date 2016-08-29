"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
whitespace
	"Strip whitespaces from the input stream."

	[ stream atEnd not and: [ stream peek isSeparator ] ]
		whileTrue: [ stream next ]
