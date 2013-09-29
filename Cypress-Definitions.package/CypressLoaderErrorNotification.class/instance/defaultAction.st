"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: handling
"
defaultAction
	"Log the notification to the GCI log and the Transcript, then resume."

	self logNotification: 'Notice: ' , self asString.
	^super defaultAction