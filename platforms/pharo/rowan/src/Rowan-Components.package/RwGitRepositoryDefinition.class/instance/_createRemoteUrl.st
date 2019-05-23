private
_createRemoteUrl

	| projectUrl remoteUrl segments |
	self projectUrl isEmpty ifTrue:  [ ^ nil ].
	projectUrl := RwUrl fromString: self projectUrl.
	remoteUrl := 'git@' , projectUrl authority , ':'.
	segments := projectUrl segments.
	remoteUrl := segments size = 1
		ifTrue: [ remoteUrl , (segments at: 1) , '.git ' ]
		ifFalse: [ remoteUrl , (segments at: 1) , '/' , (projectUrl segments at: 2) , '.git ' ].
	^ remoteUrl