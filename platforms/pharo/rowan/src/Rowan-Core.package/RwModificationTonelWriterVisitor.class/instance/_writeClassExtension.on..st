class extension writing
_writeClassExtension: aClassExtension on: aStream

	aStream << 'Extension '
				<< (self _toSTON: {(#'name' -> aClassExtension name)} asDictionary) << self _newLine.