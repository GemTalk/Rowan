private
_expected_rowanSample7_component_configuration

	^ 'RwComponentLoadConfiguration {
	#name : ''Main'',
	#comment : '''',
	#projectName : ''RowanSample7_component'',
	#version : ''0.1.0'',
	#conditionalPackages : {
		[
			''common''
		] : {
			''core'' : {
				#packageNames : [
					''RowanSample7_component-Core'',
					''RowanSample7_component-Extension''
				]
			},
			''tests'' : {
				#packageNames : [
					''RowanSample7_component-Tests_Main''
				]
			}
		},
		[
			''gemstone''
		] : {
			''core'' : {
				#packageNames : [
					''RowanSample7_component-GemStone-Core''
				]
			},
			''tests'' : {
				#packageNames : [
					''RowanSample7_component-Tests-GemStone_Main''
				]
			}
		}
	}
}'