module PCBs

using Circuits

module Lisp
include("kicad_lisp.jl")
end
using .Lisp

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

zone_settings() = Dict(:tstamp => 0,
                       :hatch => [:edge, 0.508],
                       :connect_pads => [:yes, Dict(:clearance => 0.508)],
                       :min_thickness => 0.254,
                       :fill => Dict(:arc_segments => 16,
                                    :thermal_gap => 0.508,
                                    :thermal_bridge_width => 0.508))

end

mutable struct Zone
    name::Union{Symbol,String}
    layer::Symbol
    polygon::Matrix{Float64}
    settings::Dict
end
function Zone(name, layer, polygon; settings = Settings.zone_settings())
    Zone(name, Symbol(layer), polygon, settings)
end

rectangular_zone(name, layer, x, y, w, h; kwargs...) =
    Zone(name, layer, [x y; x y+h; x+w y+h; x+w y]; kwargs...)

struct Segment
    layer::Symbol
    net::Union{Symbol,String}
    coordinates::Matrix{Float64}
    width::Float64
end
Segment(layer::Union{String,Symbol}, net::Union{String,Symbol},
        x, y, dx, dy, width) =
            Segment(Symbol(layer), net, [x y; x+dx y+dy], width)

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
    zones::Vector{Zone}
    segments::Vector{Segment}
end

PCB(circuit::Circuit, filename::String;
    directories=[],
    version="20171130",
    host="(5.0.1-3-g963ef8bb5)",
    general=Settings.general(),
    page="A4",
    layers=Settings.layers(),
    setup=Settings.setup(),
    net_classes=Settings.net_classes(),
    zones=Zone[], segments=Segment[]) =
        PCB(circuit, filename, directories,
            version, host,
            general, page, layers, setup, net_classes,
            zones, segments)

function settings_dict_to_lisp(name::String, settings::Dict, sorted::Bool=false;
                               ks=collect(keys(settings)))
    node = Lisp.KNode(Lisp.KSym(name))
    sorted && sort!(ks)
    children = map(ks) do k
        v = settings[k]
        child = Lisp.KNode(Lisp.obj(k))
        if v isa Vector
            for e in v
                if e isa Dict
                    s = settings_dict_to_lisp("tmp", e)
                    append!(child.children, s.children[2:end])
                else
                    push!(child.children, Lisp.obj(e))
                end
            end
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

function define_nets(pcb::PCB)
    net_nodes = Lisp.KNode[]
    nets = node_names(pcb.circuit)
    unique_nets = unique(nets)

    pcb.general[:nets] += length(unique_nets)

    push!(net_nodes, settings_dict_to_lisp("general", pcb.general))
    append!(net_nodes, Lisp.Read("(page $(pcb.page))"))
    push!(net_nodes, settings_dict_to_lisp("layers", pcb.layers, true))
    push!(net_nodes, settings_dict_to_lisp("setup", pcb.setup))

    append!(net_nodes, Lisp.Read("(net 0 \"\")"))
    for (i,net) in enumerate(unique_nets)
        net_node = Lisp.KNode(ksym"net", Lisp.KInt(i), Lisp.obj(net))
        push!(net_nodes, net_node)
    end

    for ((name,descr),v) in pcb.net_classes
        nc = Lisp.KNode(ksym"net_class", Lisp.KSym(name), Lisp.KStr(descr))
        append!(nc.children, settings_dict_to_lisp("net_class", v).children[2:end])
        if name == :Default
            for (i,net) in enumerate(unique_nets)
                append!(nc.children, Lisp.Read("(add_net \"$(net)\")"))
            end
        end
        push!(net_nodes, nc)
    end
    net_nodes,nets,unique_nets
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

xy(x, y) = Lisp.KNode(ksym"xy", Lisp.obj(x), Lisp.obj(y))
xyz(x, y, z) = Lisp.KNode(ksym"xyz", Lisp.obj(x), Lisp.obj(y), Lisp.obj(z))

function get_model(filename::String; at=[0,0,0], scale=[1,1,1], rotate=[0,0,0])
    [ksym"model", Lisp.KSym(filename),
     Lisp.KNode(ksym"at", xyz(at...)),
     Lisp.KNode(ksym"scale", xyz(scale...)),
     Lisp.KNode(ksym"rotate", xyz(rotate...))]
end

get_model(model::Pair{String,<:Dict{Symbol}}) = get_model(model[1];model[2]...)

function replace_layers!(footprint::Lisp.KNode, layers::Vector{Pair{Regex,String}})
    mirror = Lisp.Read("(justify mirror)")
    do_mirror = any(map(isequal("B"), last.(layers)))
    Lisp.traverse_nodes(footprint) do node
        fc = first(node.children)
        if fc isa Lisp.KSym
            if fc.name ∈ ["layer","layers"]
                node.children = copy(node.children)
                for i = 2:length(node.children)
                    for (pat,subs) in layers
                        m = match(pat, node.children[i].name)
                        if m != nothing
                            node.children[i] = Lisp.KSym("$(subs).$(m[1])")
                            break # We don't want the reverse pattern to apply as well
                        end
                    end
                end
            elseif fc.name == "fp_text"
                node.children = copy(node.children)
                Lisp.traverse_nodes(node, "effects") do text_prop
                    text_prop.children = copy(text_prop.children)
                    append!(text_prop.children, mirror)
                end
            end
        end
    end
end

function replace_layers!(footprint::Lisp.KNode, layers::Vector{Pair{String,String}})
    layers = vcat(map(l -> Regex("$(l[1])\\.(.*)") => l[2], layers),
                  map(l -> Regex("$(l[2])\\.(.*)") => l[1], layers))
    replace_layers!(footprint, layers)
end

replace_layers!(footprint::Lisp.KNode, layers::Pair{String,String}) =
    replace_layers!(footprint, [layers])

function modify_text!(footprint::Lisp.KNode, text::Dict{String,<:Dict})
    Lisp.traverse_nodes(footprint, "fp_text") do node
        text_name = node.children[2].name
        for t in keys(text)
            if text_name == t
                node.children = copy(node.children)
                for (i,n) in enumerate(node.children)
                    if n isa Lisp.KNode
                        s = Symbol(n.children[1].name)
                        s ∈ keys(text[t]) || continue
                        node.children[i] = Lisp.KNode(vcat(Lisp.KSym(s), Lisp.obj(text[t][s])))
                    end
                end
            end
        end
    end
end

function elements_to_lisp(pcb::PCB, nets, unique_nets)
    footprints = Dict{String, Lisp.KNode}()
    element_nodes = Lisp.KNode[]

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

        at = Lisp.KNode(ksym"at")
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
            if c isa Lisp.KNode && length(c.children) > 2
                if c.children[1] == ksym"pad" && c.children[2] isa Lisp.KInt
                    pad = pads[c.children[2].i]
                    c.children = copy(c.children)
                    push!(c.children,
                          Lisp.KNode(ksym"net", Lisp.KInt(pad[2]), Lisp.KStr(pad[1])))
                elseif c.children[1] == ksym"model" && :model ∈ ks
                    c.children = get_model(e.meta[:model])
                elseif c.children[1] == ksym"fp_text" &&
                    c.children[2] == ksym"reference"
                    c.children = copy(c.children)
                    c.children[3] = Lisp.KStr(string(e))
                end
            end
        end
        :layers ∈ ks && replace_layers!(footprint, e.meta[:layers])
        :text ∈ ks && modify_text!(footprint, e.meta[:text])

        push!(element_nodes, footprint)
    end
    element_nodes
end

function segments_to_lisp(pcb::PCB, nets, unique_nets)
    segment_nodes = Lisp.KNode[]
    for segment in pcb.segments
        cos = segment.coordinates
        for i = 1:size(cos,1)-1
            push!(segment_nodes,
                  Lisp.KNode(ksym"segment",
                             Lisp.KNode(ksym"start", Lisp.obj.(cos[i,:])...), Lisp.KNode(ksym"end", Lisp.obj.(cos[i+1,:])...),
                             Lisp.KNode(ksym"width", Lisp.obj(segment.width)),
                             Lisp.KNode(ksym"layer", Lisp.obj(segment.layer)),
                             Lisp.KNode(ksym"net", Lisp.obj(findfirst(isequal(segment.net), unique_nets)))))
        end
    end
    segment_nodes
end

function zones_to_lisp(pcb::PCB, nets, unique_nets)
    zone_nodes = Lisp.KNode[]
    for zone in pcb.zones
        zone_node = settings_dict_to_lisp("zone", zone.settings)
        insert!(zone_node.children, 2, Lisp.KNode(ksym"layer", Lisp.KSym(zone.layer)))
        insert!(zone_node.children, 2, Lisp.KNode(ksym"net_name", Lisp.KSym(zone.name)))
        insert!(zone_node.children, 2, Lisp.KNode(ksym"net", Lisp.KInt(findfirst(isequal(zone.name), unique_nets))))
        polygon = Lisp.KNode(ksym"polygon", Lisp.KNode(ksym"pts"))
        for i = 1:size(zone.polygon,1)
            push!(polygon.children[2].children, xy(zone.polygon[i,:]...))
        end
        push!(zone_node.children, polygon)
        push!(zone_nodes, zone_node)
    end
    zone_nodes
end

function Base.show(io::IO, pcb::PCB)
    node = Lisp.Read("(kicad_pcb (version $(pcb.version)) (host pcbnew \"$(pcb.host)\"))")[1]

    net_nodes,nets,unique_nets = define_nets(pcb)
    append!(node.children, net_nodes)
    append!(node.children, elements_to_lisp(pcb, nets, unique_nets))
    append!(node.children, segments_to_lisp(pcb, nets, unique_nets))
    append!(node.children, zones_to_lisp(pcb, nets, unique_nets))

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
