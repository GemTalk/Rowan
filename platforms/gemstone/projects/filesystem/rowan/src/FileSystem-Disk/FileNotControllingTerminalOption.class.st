"
Dont let pathname become the controlling terminal.

If the file being opened is a terminal device, prevent it from becoming the controlling terminal. If the file being opened is not a terminal, this flag has no effect.
"
Class {
	#name : 'FileNotControllingTerminalOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileNotControllingTerminalOption class >> posixName [
	^'O_NOCTTY'
]
