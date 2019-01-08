"
If the file doesnt already exist, it is created as a new, empty file. This flag is effective even if the file is being opened only for reading. If we specify O_CREAT, then we must supply a mode argument in the open() call; otherwise, the permissions of the new file will be set to some random value from the stack.
"
Class {
	#name : 'FileCreateOption',
	#superclass : 'FileCreationOption',
	#category : 'FileSystem-Disk-Options'
}

{ #category : 'accessing' }
FileCreateOption class >> defaultPlatformValue [
	^16r00040
]

{ #category : 'accessing' }
FileCreateOption class >> posixName [
	^'O_CREAT'
]

{ #category : 'accessing' }
FileCreateOption class >> windowsValue [
	^16r20000
]

{ #category : 'convenience' }
FileCreateOption >> createRule [
	^self parent fileExclusiveOption ifNil: [self store class openAlwaysRule] ifNotNil: [:option | self store class createNewRule]
]
