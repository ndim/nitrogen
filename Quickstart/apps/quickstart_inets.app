{application, quickstart_inets, [
	{description,  "Nitrogen Quickstart Examples"},
	{mod, {quickstart, []}},
	{env, [
		{platform, inets},
		{port, 8888},
		{session_timeout, 20},
		{sign_key, "b37ca07"},
		{wwwroot, "./wwwroot"}
	]}
]}.