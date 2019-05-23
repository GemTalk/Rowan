private
_readObjectFrom: aFileReference

	aFileReference readStreamDo: [:stream |
		| reader |
		reader := STON reader on:  (ZnBufferedReadStream on:  stream).
		reader classes at: #'Package' put: Dictionary.
		^ reader next ]