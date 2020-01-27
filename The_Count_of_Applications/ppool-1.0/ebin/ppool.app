{application, ppool, [
    {description, "generic sized process pool with queue"},
    {vsn, "1.0.0"},
    {modules, [ppool, ppool_supersup, ppool_sup, ppool_worker_sup, ppool_serv]},
    {registered, [ppool]},
    {mod, {ppool, []}}
]}.