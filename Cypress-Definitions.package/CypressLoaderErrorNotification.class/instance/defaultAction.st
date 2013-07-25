handling
defaultAction
	"Log the notification to the GCI log and the Transcript, then resume."

	self logNotification: 'Notice: ' , self asString.
	^super defaultAction