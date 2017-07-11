"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: git commands
"
runGitCommand: argsArray in: gitRootPath

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream nextPutAll: 'cd ' , gitRootPath , '; git '.
	argsArray do: 
			[:arg |
			stream
				space;
				nextPutAll: arg].
	^self performOnServer: stream contents