"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
reportAnyErrors
	"We're currently not collecting errors, but once we do, this will be the place they are reported.
	Answer true if errors detected (and exception resumed), false otherwise."

	^errors isEmpty
		ifFalse: 
			[self halt: 'Errors detected. Aborting application of patch.'.
			true]
		ifTrue: [false]