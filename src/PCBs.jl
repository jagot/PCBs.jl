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
    filename::String
    version::String
    host::String
    general::Dict{Symbol,Number}
    page::String
    layers::Dict{Int,Vector{Symbol}}
    setup::Dict{Symbol,Any}
    net_classes::Dict{Tuple{Symbol,String},Dict{Symbol,Float64}}
end

PCB(filename;
    version="20171130",
    host="(5.0.1-3-g963ef8bb5)",
    general=Settings.general(),
    page="A4",
    layers=Settings.layers(),
    setup=Settings.setup(),
    net_classes=Settings.net_classes()) =
        PCB(filename, version, host, general, page, layers, setup,
            net_classes)

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

function Base.show(io::IO, pcb::PCB)
    node = Lisp.Read("(kicad_pcb (version $(pcb.version)) (host pcbnew \"$(pcb.host)\"))")[1]

    push!(node.children, settings_dict_to_lisp("general", pcb.general))
    append!(node.children, Lisp.Read("(page $(pcb.page))"))
    push!(node.children, settings_dict_to_lisp("layers", pcb.layers, true))
    push!(node.children, settings_dict_to_lisp("setup", pcb.setup))

    append!(node.children, Lisp.Read("(net 0 \"\")"))

    for ((name,descr),v) in pcb.net_classes
        nc = Lisp.KNode([Lisp.KSym("net_class"), Lisp.KSym(name), Lisp.KStr(descr)])
        append!(nc.children, settings_dict_to_lisp("net_class", v).children[2:end])
        push!(node.children, nc)
    end

    show(io, node)
end

function save(pcb::PCB, c::Circuit)
    filename = "$(pcb.filename).kicad_pcb"
    @info "Saving $c to $filename"
    open(filename, "w") do file
        show(file, pcb)
    end
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
