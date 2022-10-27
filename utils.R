deserialize_from_string <- function (str64) 
{
  if (!inherits(str64, "list")) 
    str64 <- as.list(str64)
  object_list <- lapply(str64, function(x) {
    con <- rawConnection(base64enc::base64decode(x), "r+")
    object <- readRDS(con)
    close(con)
    object
  })
  if (length(object_list) == 1) 
    object_list <- object_list[[1]]
  return(object_list)
}

find_schema_by_factor_name <- function (ctx, factor.name) 
{
  visit.relation = function(visitor, relation) {
    if (inherits(relation, "SimpleRelation")) {
      visitor(relation)
    }
    else if (inherits(relation, "CompositeRelation")) {
      visit.relation(visitor, relation$mainRelation)
      lapply(relation$joinOperators, function(jop) {
        visit.relation(visitor, jop$rightRelation)
      })
    }
    else if (inherits(relation, "WhereRelation") || inherits(relation, 
                                                             "RenameRelation") || inherits(relation, "GatherRelation")) {
      visit.relation(visitor, relation$relation)
    }
    else if (inherits(relation, "UnionRelation")) {
      lapply(relation$relations, function(rel) {
        visit.relation(visitor, rel)
      })
    }
    else {
      stop(paste0("find.schema.by.factor.name unknown relation ", 
                  class(relation)))
    }
    invisible()
  }
  myenv = new.env()
  add.in.env = function(object) {
    myenv[[toString(length(myenv) + 1)]] = object$id
  }
  visit.relation(add.in.env, ctx$query$relation)
  schemas = lapply(as.list(myenv), function(id) {
    ctx$client$tableSchemaService$get(id)
  })
  Find(function(schema) {
    !is.null(Find(function(column) column$name == factor.name, 
                  schema$columns))
  }, schemas)
}