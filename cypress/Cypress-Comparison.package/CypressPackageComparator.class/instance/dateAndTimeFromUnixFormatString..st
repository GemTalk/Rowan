"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
dateAndTimeFromUnixFormatString: aString
	"YYYY-MM-DDTHH:MM:SS +HHMM
	 Examples:
		| string |
		string := '2013-06-20 14:47:55.40271592140198 -0700'.
		(DateAndTimeANSI fromUnixFormatString: string) printString = '2013-06-20T14:47:55.40271592140198-07:00'.
	"

	| stream sign positionBias |
	stream := ReadStreamPortable on: aString.
	sign := aString at: aString size - 4.
	positionBias := stream class isLegacyStreamImplementation
				ifTrue: [1]
				ifFalse: [0].
	^DateAndTime
		year: (stream next: 4) asNumber
		month: (stream
				next;
				next: 2) asNumber
		day: (stream
				next;
				next: 2) asNumber
		hour: (stream
				next;
				next: 2) asNumber
		minute: (stream
				next;
				next: 2) asNumber
		second: (stream
				next;
				next: aString size - 6 - stream position + positionBias) asNumber
		offset: (Duration
				days: 0
				hours: (stream
						next;
						next;
						next: 2) asNumber
						* (sign == $- ifTrue: [-1] ifFalse: [1])
				minutes: (stream next: 2) asNumber
				seconds: 0)