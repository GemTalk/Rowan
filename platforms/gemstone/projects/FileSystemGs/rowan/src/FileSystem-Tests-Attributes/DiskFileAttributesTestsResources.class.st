"
DiskFileAttributesTestsResources provides a file with known attributes that can be used to test attribute retrieval in the File System.

DiskFileAttributesTestsResources is currently used by FileReferenceAttributeTests.

    Instance Variables
	afterCreationTime:	The time just after file was created	<DateAndTime>
	beforeCreationTime:	The time just before file was created	<DateAndTime>
	file:					The name of the file (with known attributes)	<FileReference>


    Implementation Points
"
Class {
	#name : 'DiskFileAttributesTestsResources',
	#superclass : 'TestResource',
	#instVars : [
		'file',
		'beforeCreationTime',
		'afterCreationTime'
	],
	#category : 'FileSystem-Tests-Attributes'
}

{ #category : 'accessing' }
DiskFileAttributesTestsResources >> afterCreationTime [
	^ afterCreationTime
]

{ #category : 'accessing' }
DiskFileAttributesTestsResources >> beforeCreationTime [
	^ beforeCreationTime
]

{ #category : 'accessing' }
DiskFileAttributesTestsResources >> file [
	^ file
]

{ #category : 'running' }
DiskFileAttributesTestsResources >> setUp [

	| ws bufferedStream days hours minutes seconds |

	file := FileLocator temp / ('FileAttributesTests-', FastUUIDGenerator next asString, '.txt').
	beforeCreationTime := DateAndTime secondsLocal: DateAndTime secondsSince2001 truncated offset: (Duration seconds: 0).
	beforeCreationTime offset: (Duration seconds: (beforeCreationTime currentTimeZone transitionAtUTC: beforeCreationTime) offsetFromUTC).
	ws := file writeStream.
	bufferedStream := ZnBufferedWriteStream on: ws.
	[
		bufferedStream nextPutAll: 'Created by FileAttributesTestsResources>>setUp '.
		beforeCreationTime printOn: bufferedStream.
	] ensure: [ bufferedStream close ].
	afterCreationTime := DateAndTime now.
	"Round up to the next second"
	days := afterCreationTime dayOfYear.
	hours := afterCreationTime hour.
	minutes := afterCreationTime minute.
	seconds := afterCreationTime second + 1.
	seconds >= 60
		ifTrue: [ 
			minutes := minutes + 1.
			seconds := seconds - 60.
			minutes >= 60
				ifTrue: [
					hours := hours + 1.
					minutes := minutes - 60.
					hours >=  24
						ifTrue: [ 
							days := days + 1.
							hours := hours - 24 ] ].].
	afterCreationTime := DateAndTime
		year: afterCreationTime year day: days hour:  hours minute: minutes second: seconds
]

{ #category : 'running' }
DiskFileAttributesTestsResources >> tearDown [

	file delete.
]
