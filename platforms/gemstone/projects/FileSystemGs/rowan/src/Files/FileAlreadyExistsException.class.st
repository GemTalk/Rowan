"
I am an exception that notifies that a file already exists.
"
Class {
	#name : 'FileAlreadyExistsException',
	#superclass : 'FileException',
	#instVars : [
		'file'
	],
	#category : 'Files'
}

{ #category : 'instance creation' }
FileAlreadyExistsException class >> signalOnFile: aFile [ 
	
	self new
		file: aFile;
		signal: aFile name "use signal: so filename shows up in exception printString"
]

{ #category : 'accessing' }
FileAlreadyExistsException >> file [
	^ file
]

{ #category : 'accessing' }
FileAlreadyExistsException >> file: aFile [
	
	file := aFile
]

{ #category : 'accessing' }
FileAlreadyExistsException >> messageText [

	^ 'File already exists: ', file basename
]
