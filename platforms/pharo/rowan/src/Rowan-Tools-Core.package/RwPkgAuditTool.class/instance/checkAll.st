other
checkAll
"check all packages regardless dirty or not"
	^(SessionTemps current at: #RwDirtyOnly otherwise: false) not
