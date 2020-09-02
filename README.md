# Ammi
Ammi is a game script format, primarily targeting **Amagami's SCF script
format** (referred to as **ASCF**). Scripts are defined in data, which can then
be compiled to different targets for use with different engines, or differently
configured script formats.

## Further plans
  * Parser for a simple language that allows mixing in raw bytes when required.
    * Should also provide sugar to do common things, like "switch to
      character+open quote+play voice line+line+end quote"
  * Pre-compilation transformations e.g. insert pauses after commas if not
    already present
  * Potentially linked: think about how to patch compiled output into ASCF files
