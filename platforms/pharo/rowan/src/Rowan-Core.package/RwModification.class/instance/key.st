Accessing
key

	| beforeKey |
	beforeKey := before key.
	^beforeKey ifNil: [after key] ifNotNil: [beforeKey]