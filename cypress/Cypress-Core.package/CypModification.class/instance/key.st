"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Accessing
"
key

	| beforeKey |
	beforeKey := before key.
	^beforeKey ifNil: [after key] ifNotNil: [beforeKey]