printing
printOn: aStream
	"Return the FileUrl according to RFC3986
		'file:'['//'<host>]<path>#<fragment>
	Note that <host> being '' is equivalent to 'localhost' and is not printed."

	aStream nextPutAll: self schemeName;
		nextPut: $:.

	"File URLs with hosts (which are fairly useless) cannot be relative."
	host isEmpty ifFalse: [isAbsolute ifFalse: [aStream nextPutAll: '<ErroneousURL>'. ^nil].
						aStream nextPutAll: '//';
						nextPutAll: host].

	aStream
		nextPutAll: self pathString.

	fragment ifNotNil:
		[aStream
			nextPut: $#;
			nextPutAll: fragment ].