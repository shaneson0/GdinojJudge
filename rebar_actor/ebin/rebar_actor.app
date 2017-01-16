{application, 'rebar_actor', [
	{description, ""},
	{vsn, "rolling"},
	{modules, ['actor_handle','rebar_actor_app','rebar_actor_sup']},
	{registered, [rebar_actor_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {rebar_actor_app, []}},
	{env, []}
]}.