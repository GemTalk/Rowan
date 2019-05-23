accessing
packageFormatIfAbsent: absentBlock

	^ self properties at: 'packageFormat' ifAbsent: absentBlock