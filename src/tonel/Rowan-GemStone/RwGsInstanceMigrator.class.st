Class {
	#name : 'RwGsInstanceMigrator',
	#superclass : 'Object',
	#instVars : [
		'migrationEnabled',
		'maxThreads',
		'maxCpuPercentage'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsInstanceMigrator class >> aggressiveMigration [

	^ self new
		maxThreads: SystemRepository _aggressiveMaxThreadCount;
		maxCpuPercentage: 95;
		yourself
]

{ #category : 'instance creation' }
RwGsInstanceMigrator class >> conservativeMigration [

	^ self new
]

{ #category : 'instance creation' }
RwGsInstanceMigrator class >> noMigration [

	^ self new
		migrationEnabled: false;
		yourself
]

{ #category : 'accessing' }
RwGsInstanceMigrator >> maxCpuPercentage [

	^ maxCpuPercentage ifNil: [ 90 ]
]

{ #category : 'accessing' }
RwGsInstanceMigrator >> maxCpuPercentage: anObject [

   maxCpuPercentage := anObject

]

{ #category : 'accessing' }
RwGsInstanceMigrator >> maxThreads [

	^ maxThreads ifNil: [ 2 ]
]

{ #category : 'accessing' }
RwGsInstanceMigrator >> maxThreads: anObject [

   maxThreads := anObject

]

{ #category : 'migration' }
RwGsInstanceMigrator >> migrateInstancesOf: aClassArray [

	| instancesArray classCount |
	self migrationEnabled
		ifFalse: [ ^ self ].
	System commit.
	instancesArray := SystemRepository
		listInstances: aClassArray
		limit: 0
		toDirectory: nil
		withMaxThreads: self maxThreads
		maxCpuUsage: self maxCpuPercentage
		memoryOnly: false.
	classCount := 1.
	1 to: instancesArray size by: 2 do: [ :index | 
		| class instances history instanceCount |
		class := aClassArray at: classCount.
		instanceCount := instancesArray at: index.
		instances := instancesArray at: index + 1.
		history := class classHistory.
		instanceCount > 0
			ifTrue: [ class migrateInstances: instances to: history last ].
		[ 1 < history size ] whileTrue: [ history first removeFromClassHistory ].
		classCount := classCount + 1 ]
]

{ #category : 'accessing' }
RwGsInstanceMigrator >> migrationEnabled [

	^ migrationEnabled ifNil: [ true ]
]

{ #category : 'accessing' }
RwGsInstanceMigrator >> migrationEnabled: anObject [

   migrationEnabled := anObject

]
