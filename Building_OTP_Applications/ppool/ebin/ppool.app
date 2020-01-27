{application, ppool, [
    {description, "generic sized process pool with queue"},
    {vsn, "0.0.1"},
    {modules, [ppool, ppool_supersup, ppool_sup, ppool_worker_sup, ppool_serv]},
    {registered, [ppool]},
    {mod, {ppool, []}}
]}.