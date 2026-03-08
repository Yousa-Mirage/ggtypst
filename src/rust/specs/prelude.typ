#let define-sym(s, sym: none) = {
  (
    (kind: "alias-sym", alias: s),
    if sym != none {
      (alias: s, handle: sym)
    } else {
      none
    },
  )
}

#let define-greedy-cmd(s, handle: none) = {
  (
    (kind: "greedy-cmd", alias: s),
    if handle != none {
      (alias: s, handle: handle)
    } else {
      none
    },
  )
}

#let define-infix-cmd(s, handle: none) = {
  (
    (kind: "infix-cmd", alias: s),
    if handle != none {
      (alias: s, handle: handle)
    } else {
      none
    },
  )
}

#let define-glob-cmd(pat, s, handle: none) = {
  (
    (kind: "glob-cmd", pattern: pat, alias: s),
    if handle != none {
      (alias: s, handle: handle)
    } else {
      none
    },
  )
}

#let define-cmd(num, alias: none, handle: none) = {
  (
    (
      kind: "cmd",
      args: ("kind": "right", "pattern": (kind: "fixed-len", len: num)),
      alias: alias,
    ),
    if handle != none {
      (alias: alias, handle: handle)
    } else {
      none
    },
  )
}

#let define-env(num, kind: "none", alias: none, handle: none) = {
  (
    (
      kind: "env",
      args: if num != none {
        (kind: "fixed-len", len: num)
      } else {
        (kind: "none")
      },
      ctx_feature: (kind: kind),
      alias: alias,
    ),
    if handle != none {
      (alias: alias, handle: handle)
    } else {
      none
    },
  )
}

#let define-glob-env(pat, kind: "none", alias: none, handle: none) = {
  (
    (
      kind: "glob-env",
      pattern: pat,
      ctx_feature: (kind: kind),
      alias: alias,
    ),
    if handle != none {
      (alias: alias, handle: handle)
    } else {
      none
    },
  )
}

#let sym = ((kind: "sym"), none)

#let of-sym(handle) = ((kind: "sym"), (handle: handle))

#let left1-op(alias) = ((kind: "cmd", args: (kind: "left1"), alias: alias), none)

#let cmd1 = ((kind: "cmd1"), none)

#let cmd2 = ((kind: "cmd2"), none)

#let matrix-env = ((kind: "matrix-env"), none)

#let process-spec(definitions) = {
  let spec = (:)
  let scope = (:)
  for (key, value) in definitions.pairs() {
    let spec-item = value.at(0)
    let scope-item = value.at(1)
    spec.insert(key, spec-item)
    if scope-item != none {
      if "alias" in scope-item and type(scope-item.alias) == str {
        let key = if scope-item.alias.starts-with("#") {
          scope-item.alias.slice(1)
        } else {
          scope-item.alias
        }
        scope.insert(key, scope-item.handle)
      } else {
        scope.insert(key, scope-item.handle)
      }
    }
  }
  (spec: spec, scope: scope)
}
