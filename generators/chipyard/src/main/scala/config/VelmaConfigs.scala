package chipyard
import freechips.rocketchip.config.{Config}
import freechips.rocketchip.diplomacy.{AsynchronousCrossing}

class PetRoCCConfig extends Config(
  new velma.HasPetRoCC ++
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++ 
  new chipyard.config.AbstractConfig)
