devtools::load_all()

reticulate::use_virtualenv("experiments")






## TODO:
## 1. proceed with gen_archs (split string ~block24)
## 2. check mail thomas
## 3. prep labels and proceed with replace effect codes











generic_archetypes = []
for c in cars:  ## is list of similar cars
  if c is None:
  generic_archetypes.append(None)
continue
ga = generate_archetype(c)
print("blobb")
generic_archetypes.append(ga)

df_archs["generic_archetypes"] = generic_archetypes

return df_archs




df <- tibblify::tibblify(test)
df$costs %>% View()
