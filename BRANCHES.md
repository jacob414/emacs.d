# Branches

This configuration runs on several machines. Each machine gets its own branch;
`master` is the shared, public part.

| Branch       | Contents                                           | Pushed to              |
|--------------|----------------------------------------------------|------------------------|
| `master`     | Generic configuration, safe to publish             | GitHub (`origin`)      |
| `<hostname>` | `master` + machine-specific or private commits     | a private remote only  |

The branch name is the machine's short hostname (`hostname -s`), e.g. `zipfly`,
`dimman`. Host selection inside Emacs still happens in `late-config.el` via
`hosts/<name>.el`; the branches only decide *which commits* a machine carries.

## Day to day

- **A change that is useful everywhere:** commit it on `master`, then merge
  `master` into each machine branch (`git merge master`).
- **A change for one machine, or one that mentions private paths, people or
  services:** commit it on that machine's branch only.
- **Started on the machine branch but it turned out generic:** `git cherry-pick`
  it onto `master`.
- Never merge a machine branch into `master`.

## The guard

`tools/pre-push-public-guard` refuses to push anything but `master` to GitHub,
and refuses `master` commits that add private-looking lines (home-directory paths,
private notes). Install it once per clone:

    ln -s ../../tools/pre-push-public-guard .git/hooks/pre-push

Extra patterns can be added locally in `.git/info/private-patterns`.
