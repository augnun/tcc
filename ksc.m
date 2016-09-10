pkg load statistics


geo = load("dados_hex/hex.geo");
pop =load("dados_hex/hex.pop");
casos = load("dados_hex/hex.cas");

pop_total = sum(pop);
casos_total = sum(casos);

mat_dist = squareform(pdist(geo));

