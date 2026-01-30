;; SPDX-License-Identifier: MPL-2.0-or-later
;; STATE.scm - Project state for stateful-artefacts-for-gitforges

(state
  (metadata
    (version "0.1.0")
    (schema-version "1.0")
    (created "2024-06-01")
    (updated "2025-01-17")
    (project "stateful-artefacts-for-gitforges")
    (repo "hyperpolymath/stateful-artefacts-for-gitforges"))

  (project-context
    (name "Stateful Artefacts for Git Forges")
    (tagline "Standardized stateful artefact management for GitHub/GitLab/Bitbucket")
    (tech-stack ("scheme" "yaml")))

  (current-position
    (phase "specification")
    (overall-completion 20)
    (working-features
      ("STATE.scm format spec"
       "META.scm format spec"
       "ECOSYSTEM.scm format spec"))))
