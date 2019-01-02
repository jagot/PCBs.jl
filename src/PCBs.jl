module PCBs

using Circuits

module Lisp
include("kicad_lisp.jl")
end

module Settings
general() = Dict{Symbol,Number}(
    :thickness => 1.6,
    :drawings => 0,
    :tracks => 0,
    :zones => 0,
    :modules => 1,
    :nets => 1)

layers() = Dict{Int,Vector{Symbol}}(
    0 => [Symbol("F.Cu"), :signal],
    31 => [Symbol("B.Cu"), :signal],
    32 => [Symbol("B.Adhes"), :user],
    33 => [Symbol("F.Adhes"), :user],
    34 => [Symbol("B.Paste"), :user],
    35 => [Symbol("F.Paste"), :user],
    36 => [Symbol("B.SilkS"), :user],
    37 => [Symbol("F.SilkS"), :user],
    38 => [Symbol("B.Mask"), :user],
    39 => [Symbol("F.Mask"), :user],
    40 => [Symbol("Dwgs.User"), :user],
    41 => [Symbol("Cmts.User"), :user],
    42 => [Symbol("Eco1.User"), :user],
    43 => [Symbol("Eco2.User"), :user],
    44 => [Symbol("Edge.Cuts"), :user],
    45 => [Symbol("Margin"), :user],
    46 => [Symbol("B.CrtYd"), :user],
    47 => [Symbol("F.CrtYd"), :user],
    48 => [Symbol("B.Fab"), :user],
    49 => [Symbol("F.Fab"), :user])

setup() = Dict{Symbol,Any}(
    :last_trace_width => 0.25,
    :trace_clearance => 0.2,
    :zone_clearance => 0.508,
    :zone_45_only => :no,
    :trace_min => 0.2,
    :segment_width => 0.2,
    :edge_width => 0.15,
    :via_size => 0.8,
    :via_drill => 0.4,
    :via_min_size => 0.4,
    :via_min_drill => 0.3,
    :uvia_size => 0.3,
    :uvia_drill => 0.1,
    :uvias_allowed => :no,
    :uvia_min_size => 0.2,
    :uvia_min_drill => 0.1,
    :pcb_text_width => 0.3,
    :pcb_text_size => [1.5, 1.5],
    :mod_edge_width => 0.15,
    :mod_text_size => [1, 1],
    :mod_text_width => 0.15,
    :pad_size => [1.524, 1.524],
    :pad_drill => 0.762,
    :pad_to_mask_clearance => 0.051,
    :solder_mask_min_width => 0.25,
    :aux_axis_origin => [0, 0],
    :visible_elements => 0xFFFFFF7F,
    :pcbplotparams => Dict{Symbol, Any}(
        :layerselection => Symbol("0x010fc_ffffffff"),
        :usegerberextensions => false,
        :usegerberattributes => false,
        :usegerberadvancedattributes => false,
        :creategerberjobfile => false,
        :excludeedgelayer => true,
        :linewidth => 0.100000,
        :plotframeref => false,
        :viasonmask => false,
        :mode => 1,
        :useauxorigin => false,
        :hpglpennumber => 1,
        :hpglpenspeed => 20,
        :hpglpendiameter => 15.000000,
        :psnegative => false,
        :psa4output => false,
        :plotreference => true,
        :plotvalue => true,
        :plotinvisibletext => false,
        :padsonsilk => false,
        :subtractmaskfromsilk => false,
        :outputformat => 1,
        :mirror => false,
        :drillshape => 1,
        :scaleselection => 1,
        :outputdirectory => ""
    )
)

net_classes() = Dict{Tuple{Symbol,String},Dict{Symbol,Float64}}(
    (:Default,"This is the default net class.") => Dict{Symbol,Float64}(
        :clearance => 0.2,
        :trace_width => 0.25,
        :via_dia => 0.8,
        :via_drill => 0.4,
        :uvia_dia => 0.3,
        :uvia_drill => 0.1
    )
)

end

struct PCB
    circuit::Circuit
    filename::String
    directories::Vector{String}
    version::String
    host::String
    general::Dict{Symbol,Number}
    page::String
    layers::Dict{Int,Vector{Symbol}}
    setup::Dict{Symbol,Any}
    net_classes::Dict{Tuple{Symbol,String},Dict{Symbol,Float64}}
end

PCB(circuit::Circuit, filename::String;
    directories=[],
    version="20171130",
    host="(5.0.1-3-g963ef8bb5)",
    general=Settings.general(),
    page="A4",
    layers=Settings.layers(),
    setup=Settings.setup(),
    net_classes=Settings.net_classes()) =
        PCB(circuit, filename, directories,
            version, host,
            general, page, layers, setup, net_classes)

function settings_dict_to_lisp(name::String, settings::Dict, sorted::Bool=false)
    node = Lisp.KNode([Lisp.KSym(name)])
    ks = collect(keys(settings))
    sorted && sort!(ks)
    children = map(ks) do k
        v = settings[k]
        child = Lisp.KNode([Lisp.obj(k)])
        if v isa Vector
            append!(child.children, Lisp.obj.(v))
        elseif v isa Dict
            child = settings_dict_to_lisp(string(k), v)
        else
            push!(child.children, Lisp.obj(v))
        end
        child
    end
    append!(node.children, children)
    node
end

function read_kicad_lisp_file(filename)
    isfile(filename) || throw(ArgumentError("$filename missing"))
    @debug "Reading $filename"
    open(Lisp.Read, filename)[1]
end

function find_footprint(footprint, directories)
    directories = copy(directories)
    push!(directories, ENV["KISYSMOD"])

    footprint_path = split(footprint, ":")
    footprint_path[1] = "$(footprint_path[1]).pretty"
    footprint_path[2] = "$(footprint_path[2]).kicad_mod"
    footprint_path = joinpath(footprint_path...)

    @debug "Finding footprint $footprint"
    for dir in directories
        candidate = joinpath(dir, footprint_path)
        @debug "Trying $candidate"
        isfile(candidate) && return read_kicad_lisp_file(candidate)
    end
    throw(ArgumentError("Footprint $footprint not found"))
end

function Base.show(io::IO, pcb::PCB)
    node = Lisp.Read("(kicad_pcb (version $(pcb.version)) (host pcbnew \"$(pcb.host)\"))")[1]

    nets = node_names(pcb.circuit)
    unique_nets = unique(nets)

    pcb.general[:nets] += length(unique_nets)

    push!(node.children, settings_dict_to_lisp("general", pcb.general))
    append!(node.children, Lisp.Read("(page $(pcb.page))"))
    push!(node.children, settings_dict_to_lisp("layers", pcb.layers, true))
    push!(node.children, settings_dict_to_lisp("setup", pcb.setup))

    append!(node.children, Lisp.Read("(net 0 \"\")"))
    for (i,net) in enumerate(unique_nets)
        net_node = Lisp.KNode([Lisp.KSym("net"), Lisp.KInt(i), Lisp.obj(net)])
        push!(node.children, net_node)
    end

    for ((name,descr),v) in pcb.net_classes
        nc = Lisp.KNode([Lisp.KSym("net_class"), Lisp.KSym(name), Lisp.KStr(descr)])
        append!(nc.children, settings_dict_to_lisp("net_class", v).children[2:end])
        if name == :Default
            for (i,net) in enumerate(unique_nets)
                append!(nc.children, Lisp.Read("(add_net \"$(net)\")"))
            end
        end
        push!(node.children, nc)
    end

    footprints = Dict{String, Lisp.KNode}()

    for e in pcb.circuit.elements
        f = try
            e.meta[:footprint]
        catch
            throw(ArgumentError("Footprint missing from $e"))
        end
        footprint = copy(get(footprints, f, find_footprint(f, pcb.directories)))
        ks = keys(e.meta)

        x = (:x ∈ ks ? e.meta[:x]
             : :centerx ∈ ks && :offsetx ∈ ks ? e.meta[:centerx] + e.meta[:offsetx]
             : 0)
        y = (:y ∈ ks ? e.meta[:y]
             : :centery ∈ ks && :offsety ∈ ks ? e.meta[:centery] + e.meta[:offsety]
             : 0)
        rot = :rot ∈ ks ? e.meta[:rot] : 0

        at = Lisp.KNode([Lisp.KSym("at")])
        for c in [x,y,rot]
            push!(at.children, Lisp.obj(c))
        end
        push!(footprint.children, at)

        pads = map(enumerate(pins(pcb.circuit, e))) do (i,pi)
            net = nets[pi]
            net_index = findfirst(isequal(net), unique_nets)
            @debug "Connecting pin $i of $e (global pin $pi) to net $(net) (#$(net_index))"
            net,net_index
        end
        for (i,c) in enumerate(footprint.children)
            if c isa Lisp.KNode && length(c.children) > 2 &&
                c.children[1] == Lisp.KSym("pad") && c.children[2] isa Lisp.KInt
                pad = pads[c.children[2].i]
                c.children = copy(c.children)
                push!(c.children,
                      Lisp.KNode([Lisp.KSym("net"), Lisp.KInt(pad[2]), Lisp.KStr(pad[1])]))
            end
        end

        push!(node.children, footprint)
    end

    show(io, node)
end

function save(pcb::PCB,overwrite::Bool=false)
    filename = "$(pcb.filename).kicad_pcb"
    if isfile(filename)
        if overwrite
            @warn "Overwriting existing $(filename)"
        else
            error("$(filename) already exists")
        end
    else
        @info "Saving $(pcb.circuit) to $filename"
    end
    open(filename, "w") do file
        show(file, pcb)
    end
end

export PCB

end # module
