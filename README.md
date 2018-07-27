# ResistorBuilder

**ResistorBuilder** is a graphical Wolfram Language application that finds
efficient ways to approximately construct a resistor with a given value
by combining standard E6 series resistors in series and parallel. It turns
out that within a decade (e.g., 100Ω to 1kΩ), it is possible to construct **any
resistance value** (within ±0.3% tolerance) using only
**four E6 series resistors**.

# Installation and Usage

ResistorBuilder is **fully self-contained** within a single file and has no
external dependencies (other than a recent version of *Mathematica*). Just
download and evaluate `ResistorBuilder.wl`, and ResistorBuilder should appear
as a `Manipulate` window at the bottom of the package window.

# Usage Examples
* Move the slider or enter a number in the text input field (between 100 and 1000) to change the target resistance.
* All resistor circuits having a combined value within ±0.3% of the target resistance are displayed underneath.
* Exact resistance values are displayed to the right of the corresponding circuits. Sometimes, there will be a circuit that exactly matches the target resistance; other times, they might all be approximate.

![ResistorBuilder examples](https://user-images.githubusercontent.com/4504054/43305711-9a00fa1c-912d-11e8-861b-239f15f22da4.png)
