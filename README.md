# OM-Harmonist



**Welcome to OM-Harmonist (Beta Version)**

OM-Harmonist is a library for OpenMusic developed in Common Lisp. It is primarily designed for automatic part-writing and harmonic analysis, along with several auxiliary functions. You can download the tutorial handbook [here](./docs/OM-Harmonist-Tutorials.pdf)

**Author:** Hangzhong Lian (https://soundcloud.com/hangzhonglian)

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

**License and Disclaimer**

This program is free software. For information on use and redistribution, please refer to the `LICENSE` file included in this distribution.

This software is provided primarily for educational and academic purposes without any warranty.

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

**Getting Started**

For instructions on how to load external libraries in OpenMusic, please consult the official IRCAM documentation.

Detailed demonstration patches on how to use this library are provided in the accompanying `tutorials` folder.

**Important Notes & Limitations**

- **Beta Version:** As a beta release, this library has not been tested on OpenMusic version 7.5 or newer. Therefore, compatibility with future versions is not guaranteed.
- **Functionality:** The part-writing and analysis functions do not yet support modulations. However, they can correctly process most forms of chromaticism, including secondary chords and Neapolitan chords. (Full modulation support is planned for a future release. Stay tuned!)

**Acknowledgements**

The display module, which handles the proper notation of accidentals in tonal music, is an expansion based on the foundational algorithms developed by composer ©Jialin Liu. This was implemented to address certain limitations within the native OpenMusic environment.

