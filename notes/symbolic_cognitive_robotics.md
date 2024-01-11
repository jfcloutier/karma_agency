# Symbolic cognitive robotics

## Formative concepts
	Active Inference (an agent actively minimizes surprise to survive)
	Enactivism (an agent's perceptions and actions are constructively co-dependent)
	Apperception (sense-making as discovery of unified causal theories)
	Mortal Computing (meaning is grounded in the agent's drive to survive)
	Society of Mind (an agent is animated by a collective of cognition actors interacting with each other and the environment)
	Constraint Closure (the cognition actors constrain how the Society of Mind can change, and vice-versa)
	Kantian Whole (the parts -cognition actors- exist for and by means of the whole -the Society of Mind-)	
	
## Introspection vs extrospection

	The object named "self" is implicitly in the CA's umwelt

	Extrospection => generation of (more or less abstracted) sensations from the external world
	Introspection => generation of cognitive sensations (internal world)
	Detectors and effectors are exposed as extrospective primitive CAs
	Every other CA can be coopted into umwelts as a generator of introspective or extrospective sensations, and as an effector of cognitive actions

## The umwelt of a CA
	A set of other CAs
	What they expose to all other CAs
		the vocabulary of their idiosyncratic beliefs (what others can make predictions about)
			extant, latent and synthetic objects (typed with extant or latent object types), 
			extant, latent and synthetic relations/properties
				a latent or synthetic property is always boolean-valued
		
			all CAs have a common vocabulary of meta-cognition beliefs
	what they emit when prompted by predictions:
		prediction errors from their beliefs
			from perceiving other CAs
			from their cognitive self-assessments/beliefs (from introspection)
		
	Perceiving:
		Discrete time step
			duration is proportional to abstraction level
		Making predictions about umwelt CA beliefs
			and getting prediction errors or not
		
	Perceptions:
		Uncontradicted predictions
		Prediction errors can be emitted in response to prediction, with precision
			If multiple CAs respond to a prediction with prediction errors
				The prediction error with the highest precision is picked
					Tie-breaking is random
		
	The precision of a prediction error (a float between 0 and 1) is a function of:
		The confidence of the emitting CA in the contradicting belief, which is a function of:
			The accuracy of the supporting causal model
			The duration of the supporting trend
				And average precision modulated by the variance in the precision of the perceptions aggregated by the trend
		
## A CA's beliefs - what's imagined, analyzed, partitioned and categorized by the CA - from its perceptions (unrefuted predictions + prediction errors about the beliefs of CAs in its umwelt)

	- Beliefs are available to other CA's as *synthetic or latent* thus *novel* perceptions
	- Beliefs are supported by a causal theory (latent) and by trends in past perceptions (synthetic)
	- Beliefs have associated normativity (pleasant vs unpleasant vs indifferent beliefs)
	
	- Latent - unobserved but imagined/abduced properties/relations/objects to (causally) make sense of observations - the thin now -
	  Synthetic - induced from, and thus supported by, perception trends - the thick now -

	* Trend analysis (trends support the synthesis of beliefs in the thick now)
			
		trend value is either stable, unstable, up or down
	
		Specific trend- trend(<predicate name>(<object name>, <object name> | <domain value>)), <trend value>, <since>) - a trend on an instance of a property/relation (stable, unstable)
		Generic trend - trend(<predicate name>(<object name>)), <trend value>, <since>) - a trends on a type of property/relation for an object (stable, unstable, up, down) - up/down for relations describe counts of related objects, for properties up/down describes rise/fall in values (property value domains are ordered)
		
		A trend and associated normativity can be preserved as long-term memory 
			compressed(<trend>, <time interval>)
			associated with beliefs (their support)
		uncompressed trends represent short-term memory (developing trend)

	* Normativity (from association with current feelings) is always about trends
		feeling(<feeling type>, good | bad | neutral)
			feeling types: hunger, fear, ennui
		trend_value(<trend>, good | bad | neutral)
		a trend takes its (emotional) value from concurrent feelings
		a belief supported by a trend takes the normative value of that trend
			a belief associated with a bad feeling is unpleasant, else it's pleasant (good) or indifferent (neutral)
		since trends have lengths, the normative values of trends have duration - e.g. a long-lasting unpleasant belief are worse than a short-lasting one
		
	* Analyzed: synthetic properties/relations (!= latent) are supported by attention-worthy (strongly felt or surprising) trends
		<synthetic property name>(<object_name>, true | false)
		<synthetic relation name>(<object name>, <object name>)
		
	* Partitioned - in(<new object name>, <object name>) - belief about composition -> object creation
		Parts are induced from detecting boundaries in an observed object.
		How are boundaries detected?
			An object has differentiable, stable sub-trends that coincide in time
			This might indicate that different parts of the object were being observed at different times
				- e.g. "patch of food" in the "ground" in the "environment" in the "world" ("self" is always in the "world")
		A part is usually not of the same object type as the whole (except for fractal objects)
		
	* Categorized: - is_a(<object_name>, <new object_type>) - beliefs about objects having synthetic object types
		Categorization of an object is supported by being believed to be "in" another object
			Synthetic object typing is supported by a coincidence of trends about the parent object
	
	* Significance of a trend
		A trend is significant if it breaks surprisingly from a previous trend, or correlates with a change in feelings
		
## Actions

	A CA exposes by name the actions it can execute
		A CA must always be capable of acting
			i.e. it has at least one effector CA in its transitive umwelt
	
	An action intent names an action that might be executed.
		
	The CA of an effector exposes primitive actions
		A wheel CA exposes the primitive actions "spin" and "reverse spin"
			
	A CA syntesizes actions from the actions exposed by CAs in its umwelt, names them and exposes them in turn
		A synthetic actions is a named list of synthetic actions
			e.g. action_2 = [action(ca_2, action_1), action(ca_2, action_1), action(ca_3, action_2)]
				an action can be repeated
				a synthetic action is, via closure, a sequence of primitive actions
				
	The action repertoire of a CA consists of	
		the actions it synthesized
		plus the distinct actions exposed by CAs in its umwelt
	
	A CA can not synthesize or retain an action B if action A is in its repertoire and B is identical to or is a sub-sequence of A	
					
	A CA can intend to take any action in its repertoire
		
	Execution of an intended action is inhibited if another CA concurrently intends an action that 
		covers it (is a super-sequence)
		is identical and has higher normative motivation
	
	What motivates the synthesis of actions by a CA, from least motivating to most motivating
		Babbling to cause "random" beliefs
			A CA uses actions it has already defined
			Or uses new ones because none in its repertoire have shown to be effective
		Evidencing a belief to impact confidence in it (thus the precsion of reported prediction errors)
			Ditto
		Eliminating an unpleasant belief
			A CA uses an action associated with removing the belief
		
	A CA intends at most one motivated action per time slice
		The most motivated action in its repertoire
		A motivation tie is randomly broken
			
	An action taken (it was intented by a CA) is available to all CAs
		during time slice T of the CA
		As a closed sequence of primitive actions
		If a sub-sequence of the observed primitive actions recreates a synthetic action of the CA
			the longest synthetic action is what is observed, plus the second longest etc.
		
	Why does a CA synthesize a new action?
		Because its to-be-composited actions are associated with a belief change (creation, verification, elimination)
		Involved how?
			Causation via participation in a causal model
			Correlation with a belief-supporting trend starting/ending/enduring
				Extract a sequence of actions that runs before/through the trend
			Babbling
				Make a variation on a synthetic action
					Amplify sub-sequences via duplication
					Tone down sub-sequences by reducing duplication
					Splice and recombine a synthetic action
				Assemble actions from the repertoire
		
	Actions and causal models
		Changes in properties/relations observed by a CA are either caused by latent processes or by actions
			In a static environment, they are caused entirely by actions!
				No perception without action, no action without perception
		To make sense of/apperceive the consequences of actions, they must be observed together with property/relation changes

	A policy is an action associated with a belief and a goal (verification, elimination)

## Feelings

	Hunger, fear, boredom	

## Constraints
 
	CA's in an umwelt can't contradict one another about what's what - consistent object subsumptions
	Abstraction is monotonic
	An umwelt is either introspective or extrospective, not both
	Only one action in a conflicting set can be executing at any given point in time
	
	Contraint closure?
	

