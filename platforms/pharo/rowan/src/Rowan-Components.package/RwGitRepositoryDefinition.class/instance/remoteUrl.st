accessing
remoteUrl

	^ self properties at: 'remoteUrl' ifAbsentPut: [ self _createRemoteUrl ]