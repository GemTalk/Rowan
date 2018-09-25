Class {
	#name : 'RwGsDeferredInstanceMigrator',
	#superclass : 'RwGsInstanceMigrator',
	#instVars : [
		'classesToMigrate'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'accessing' }
RwGsDeferredInstanceMigrator >> classesToMigrate [

	^ classesToMigrate

]

{ #category : 'migration' }
RwGsDeferredInstanceMigrator >> migrateInstancesOf: aClassArray [

	"Record the list of classes with new class versions"

	classesToMigrate := aClassArray

]
