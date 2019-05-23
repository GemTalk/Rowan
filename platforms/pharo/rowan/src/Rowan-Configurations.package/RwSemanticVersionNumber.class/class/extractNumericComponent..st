private
extractNumericComponent: subString
    "$. separated components are integers"

    | number stream |
	stream := subString readStream.
	number := [ Integer fromStream: stream ] on: Error do: [:ex | ^ subString ].
	^ stream atEnd
		ifTrue: [ 
			(subString size > 1 and: [ (subString at: 1) = $0 ])
				ifTrue: [ self error: 'invalid version number: numberic components may not have a leading 0' ]
				ifFalse: [ number ] ]
		ifFalse: [ subString ]