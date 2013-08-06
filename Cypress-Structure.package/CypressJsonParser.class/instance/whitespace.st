"
notice: This work is protected by copyright. All rights reserved.
category: private
"
whitespace
	"Strip whitespaces from the input stream."

	[ stream atEnd not and: [ stream peek isSeparator ] ]
		whileTrue: [ stream next ]
