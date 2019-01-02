module PCBs

using Circuits

module Lisp
include("kicad_lisp.jl")
end

struct PCB
    filename::String
end

function save(pcb::PCB, c::Circuit)
    @info "Saving $c to $pcb"
end

function read_kicad_lisp_file(filename)
    isfile(filename) || throw(ArgumentError("$filename missing"))
    @info "Reading $filename"
    contents = open(Lisp.Read, filename)
end

function read_kicad_footprint(footprint, kicad_footprint_dir)
    footprint = split(footprint, ":")
    footprint[1] = "$(footprint[1]).pretty"
    footprint[2] = "$(footprint[2]).kicad_mod"
    read_kicad_lisp_file(joinpath(kicad_footprint_dir, footprint...))
end

export PCB

end # module
