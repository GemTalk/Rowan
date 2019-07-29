Class {
	#name : 'RwInvalidMethodProtocolConventionErrorNotification',
	#superclass : 'RwInvalidCategoryProtocolConventionErrorNotification',
	#instVars : [
		'methodDefinition',
		'className',
		'isMeta'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwInvalidMethodProtocolConventionErrorNotification class >> signalWithMethodDefinition: aMethodDefinition className: className isMeta: isMeta packageName:  packageName packageConvention: aString [

	^ self new
			methodDefinition: aMethodDefinition;
			className: className;
			isMeta: isMeta;
			packageName:  packageName;
			packageConvention: aString;
			signal
]

{ #category : 'private' }
RwInvalidMethodProtocolConventionErrorNotification >> _errorMessage [

	^ 'The method protocol ', 
		self methodDefinition protocol printString, 
		' for the method ', 
		self _methodPrintString printString, 
		' in the package ', 
		self packageName printString,
		' violates the package convention ', 
		self packageConvention printString, 
		'. '
]

{ #category : 'private' }
RwInvalidMethodProtocolConventionErrorNotification >> _methodPrintString [

	^ self className, (self isMeta ifTrue: [ ' class>>' ] ifFalse: [ '>>' ]), self methodDefinition selector
]

{ #category : 'accessing' }
RwInvalidMethodProtocolConventionErrorNotification >> className [

	^ className
]

{ #category : 'accessing' }
RwInvalidMethodProtocolConventionErrorNotification >> className: aString [

	className := aString
]

{ #category : 'Handling' }
RwInvalidMethodProtocolConventionErrorNotification >> defaultAction [
 
	^ Error signal: self _errorMessage
]

{ #category : 'accessing' }
RwInvalidMethodProtocolConventionErrorNotification >> isMeta [

	^ isMeta
]

{ #category : 'accessing' }
RwInvalidMethodProtocolConventionErrorNotification >> isMeta: aBool [

	isMeta := aBool
]

{ #category : 'accessing' }
RwInvalidMethodProtocolConventionErrorNotification >> methodDefinition [

	^ methodDefinition
]

{ #category : 'accessing' }
RwInvalidMethodProtocolConventionErrorNotification >> methodDefinition: aMethodDef [

	methodDefinition := aMethodDef
]
