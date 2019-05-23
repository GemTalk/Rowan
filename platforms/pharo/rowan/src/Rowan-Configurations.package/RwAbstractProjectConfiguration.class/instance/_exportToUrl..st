exporting
_exportToUrl: fileUrl

	| url |
	url := fileUrl asRwUrl.
	url schemeName = 'file'
		ifTrue: [ 
			Rowan fileUtilities
				writeStreamFor: self name , '.ston'
				in: url pathForDirectory
				do: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
  url schemeName = 'memory'
    ifTrue: [ 
		(FileSystem currentMemoryFileSystem workingDirectory / url pathForDirectory / self name , 'ston')
			writeStreamDo: [ :stream | 
			  | string |
			  string := STON toStringPretty: self.
			  stream nextPutAll: string.
			  ^ self ] ].
	^ nil	"otherwise a noop"