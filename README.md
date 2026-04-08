# F1 Net Zero 2030 - Systems Simulation Dashboard

Interactive R Shiny dashboard modeling whether Formula One can achieve net-zero carbon emissions by 2030 under different policy scenarios.

Companion tool for:

> Nandurkar, S. and Grady, C. (2026). "Modeling Formula One's Path to Net Zero by 2030: A Systems Approach for Evaluating Decarbonization Strategies." *Frontiers in Climate*. [DOI]

## Features

- **Static What-If Calculator** — Set policy levers directly and see instant emission breakdowns
- **Time-Dynamic Simulation** — Project adoption trajectories from 2024 to 2030 with emissions-pressure feedback
- Four preset scenarios: 2018 Baseline, 2023 Level, 2024 Level, Net Zero
- Interactive causal loop diagram, donut charts, and stacked bar comparisons
- Light/dark theme toggle

## Policy Levers

| Lever | Range |
|-------|-------|
| SAF adoption | 0 -- 100% |
| Rail/sea freight share | 0 -- 60% |
| Remote work | 0 -- 100% |
| Renewable electricity | 0 -- 100% |
| Carbon offset rate | 0 -- 100% |
| Calendar size | 18 -- 26 races |
| Calendar regionalisation | 0 -- 100% |

## Run Locally

```bash
Rscript -e 'shiny::runApp("f1_net_zero_simulator.R")'
```

Requires R with the `shiny` package installed.

## Deploy

A `Dockerfile` is included for container deployment (e.g., Railway, Render, or any Docker host).

## License

MIT License. See [LICENSE](LICENSE).

## Citation

If you use this dashboard or its underlying model, please cite the accompanying paper.
