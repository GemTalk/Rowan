Class {
	#name : 'BaselineOfRowan',
	#superclass : 'BaselineOf',
	#instVars : [ ],
	#category : 'BaselineOfRowan'
}

{ #category : 'baseline' }
BaselineOfRowan >> baseline: spec [
	<baseline>
	spec
		for: #'pharo'
		do: [
			spec
				package: 'Rowan-Components';
				package: 'Rowan-Kernel';
				package: 'Rowan-Tools-Core';
				package: 'Rowan-Configurations';
				package: 'Rowan-Definitions';
				package: 'Rowan-Core-Definitions-Extensions';
				package: 'Rowan-Core';
				package: 'Rowan-Tools-Extensions';
				package: 'Rowan-Specifications';
				package: 'Rowan-Url-Extensions';
				package: 'Rowan-Url-Core';
				package: 'Rowan-Components-Kernel';
				package: 'Rowan-Tests';
				yourself ]
]

{ #category : 'accessing' }
BaselineOfRowan >> project [

	^ (super project)
			loadType: #atomic;
			yourself
]
