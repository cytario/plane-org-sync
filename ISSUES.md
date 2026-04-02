# Issues

No open issues.

## Resolved

### Duplicates (fixed in 70b0e57)

Every pull used to re-add already existing entries when `PLANE_PROJECT_ID`
was missing from headings. Fixed by including headings with nil/empty
`PLANE_PROJECT_ID` in the per-project heading filter, and by repairing
missing metadata on pull.
