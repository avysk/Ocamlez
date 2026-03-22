# Ocamlez

An OCaml visualization of the **Lorenz attractor** - a chaotic dynamical system discovered by meteorologist Edward Lorenz.

## About

This program renders a 2D projection of the Lorenz attractor's trajectory using the OCaml Graphics library. The attractor emerges from a simplified model of atmospheric convection and exhibits sensitive dependence on initial conditions (the "butterfly effect").

## Prerequisites

- OCaml 4.14.1+
- opam

## Quick Start

```bash
# Initialize environment (creates local opam switch)
make init
eval $(opam env)

# Build and run
make run
```

Press `Escape` to close the visualization window.

## Build Commands

| Command | Description |
|---------|-------------|
| `make init` | Create local opam switch and install dependencies |
| `make` | Build the project |
| `make clean` | Remove build artifacts |
| `make run` | Build and execute |

## The Lorenz System

The attractor is defined by three differential equations:

```
dx/dt = s(y - x)
dy/dt = x(r - z) - y
dz/dt = xy - bz
```

### Parameters

| Parameter | Value | Description |
|-----------|-------|-------------|
| `s` | 10.0 | Prandtl number |
| `b` | 8/3 | Geometric factor |
| `r` | 28.0 | Rayleigh number |

The Rayleigh number (`r`) can be varied to produce different attractor shapes.

### Visualization Settings

- `size`: 500px window
- `scale`: 5.0 (rendering scale factor)
- `generations`: 100,000 iterations
- `dt`: 0.001 (time step)

## License

MIT
