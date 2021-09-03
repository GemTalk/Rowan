"
I am used during upgrade to force method recompilation during a load when 
all methods must be recompiled (i.e., upgrade vrom 3.2.15 to 3.6.2)
"
Class {
	#name : 'RwPropertyModificationForUpgrade',
	#superclass : 'RwPropertyModification',
	#category : 'Rowan-Core'
}

{ #category : 'testing' }
RwPropertyModificationForUpgrade >> isEmpty [
"guarantee that even though the oldValue and newValue are #=, we answer false
	which forces the modification to be applied"

	^ false
]
