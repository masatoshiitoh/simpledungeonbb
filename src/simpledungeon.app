{application, simpledungeon,
 [{vsn,"0.0.1"},
  {modules,
		[
		battle,
		battle_mgr,
		character,
		character_stream,
		con_yaws,
		db,
		mmoasp,
		morningcall,
		mout,
		move_old,
		notice_mgr,
		npc,
		path_finder,
		simpledungeon,
		task,
		test,
		throw,
		u,
		unarmed
		]},

  {registered, [simpledungeon]},
  {mod, {simpledungeon, []}}
]}.
