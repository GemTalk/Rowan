"
3.2.15:

listInstances:
listInstances:limit:
listInstances:limit:toDirectory:withMaxThreads:maxCpuUsage:
listInstances:limit:toDirectory:withMaxThreads:maxCpuUsage:memoryOnly:
listInstances:toDirectory:
listInstancesInMemory:
listInstancesInPageOrder:toFile:
listInstancesInPageOrder:toFile:withMaxThreads:maxCpuUsage:
listInstancesToHiddenSet:
listInstancesToHiddenSet:withMaxThreads:maxCpuUsage:

3.4.0
listInstances:
listInstances:limit:
listInstances:limit:toDirectory:withMaxThreads:maxCpuUsage:
listInstances:limit:toDirectory:withMaxThreads:maxCpuUsage:memoryOnly:
listInstances:toDirectory:
listInstancesInMemory:
listInstancesInPageOrder:toFile:
listInstancesInPageOrder:toFile:withMaxThreads:maxCpuUsage:
listInstancesToHiddenSet:
listInstancesToHiddenSet:withMaxThreads:maxCpuUsage:

"
Class {
	#name : 'RwGsInstanceMigrator',
	#superclass : 'Object',
	#instVars : [
		'migrationEnabled',
		'instanceLimit',
		'maxThreads',
		'maxCpuPercentage',
		'useHiddenSet'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsInstanceMigrator class >> aggressiveHiddenSetMigration [

	^ self new
		instanceLimit: 0;
		maxThreads: SystemRepository _aggressiveMaxThreadCount;
		maxCpuPercentage: 95;
		useHiddenSet: true;
		yourself
]

{ #category : 'instance creation' }
RwGsInstanceMigrator class >> aggressiveMigration [

	^ self new
		instanceLimit: 0;
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
RwGsInstanceMigrator >> instanceLimit [

	^ instanceLimit ifNil: [ 0 ]
]

{ #category : 'accessing' }
RwGsInstanceMigrator >> instanceLimit: anObject [

   instanceLimit := anObject

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
RwGsInstanceMigrator >> migrateInstancesOf: aClassList [

	self migrationEnabled
		ifFalse: [ ^ self ].
	self useHiddenSet
		ifTrue: [ ^ self migrateUsingHiddentSetInstancesOf: aClassList ].
	self halt
]

{ #category : 'migration' }
RwGsInstanceMigrator >> migrateUsingHiddentSetInstancesOf: aClassList [

	self halt
]

{ #category : 'accessing' }
RwGsInstanceMigrator >> migrationEnabled [

	^ migrationEnabled ifNil: [ true ]
]

{ #category : 'accessing' }
RwGsInstanceMigrator >> migrationEnabled: anObject [

   migrationEnabled := anObject

]

{ #category : 'accessing' }
RwGsInstanceMigrator >> useHiddenSet [

	^ useHiddenSet ifNil: [ false ]
]

{ #category : 'accessing' }
RwGsInstanceMigrator >> useHiddenSet: anObject [

   useHiddenSet := anObject

]
