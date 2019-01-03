using Automa
import Automa.RegExp: @re_str
const re = Automa.RegExp

# Based on the tutorial found at https://biojulia.net/Automa.jl/latest/#Overview-1

# Describe patterns in regular expression.
oct      = re"0o[0-7]+"
dec      = re"[-+]?[0-9]+"
# Hex numbers are actually present without the `0x` prefix, but the
# regexp without `0x` would conflict with `dec`.
hex      = re"[0-9A-Fa-f]+"
prefloat = re"[-+]?([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)"
float    = prefloat | re.cat(prefloat | re"[-+]?[0-9]+", re"[eE][-+]?[0-9]+")
number   = oct | dec | hex | float
numbers  = re.cat(re.opt(number), re.rep(re" +" * number), re" *")

# # Register action names to regular expressions.
# number.actions[:enter] = [:mark]
# oct.actions[:exit]     = [:oct]
# dec.actions[:exit]     = [:dec]
# hex.actions[:exit]     = [:hex]
# float.actions[:exit]   = [:float]

open_paren = re"\("
close_paren = re"\)"
paren = open_paren | close_paren

# White space is correctly identified, but for some reason not
# captured.
whitespace = re"[ \n]+"

delim = paren | whitespace
delim.actions[:enter] = [:mark]
open_paren.actions[:exit] = [:open_paren]
close_paren.actions[:exit] = [:close_paren]
whitespace.actions[:exit] = [:whitespace]

str = re"\".*\""
str.actions[:enter] = [:mark]
str.actions[:exit] = [:str]

symbol = re"[a-zA-Z\*$%]"*re.rep(re"[^ ()]")
# symbol.actions[:enter] = [:mark]
# symbol.actions[:exit] = [:symbol]

num_or_sym = number | symbol
num_or_sym.actions[:enter] = [:mark]
oct.actions[:exit]     = [:oct]
dec.actions[:exit]     = [:dec]
hex.actions[:exit]     = [:hex]
float.actions[:exit]   = [:float]
symbol.actions[:exit] = [:symbol]

# kicad_lisp_tokens = number | symbol | str
kicad_lisp_tokens = num_or_sym | str

kicad_lisp = re.cat(re.opt(kicad_lisp_tokens),
                    re.rep(delim|delim*kicad_lisp_tokens),
                    re" *")

# # Compile a finite-state machine.
machine = Automa.compile(kicad_lisp)

# # This generates a SVG file to visualize the state machine.
# write("kicad_lisp.dot", Automa.machine2dot(machine))
# run(`dot -Tpng -o kicad_lisp.png kicad_lisp.dot`)

# Bind an action code for each action name.
actions = Dict(
    :mark  => :(mark = p),
    :oct   => :(emit(:oct)),
    :dec   => :(emit(:dec)),
    :hex   => :(emit(:hex)),
    :float => :(emit(:float)),
    :open_paren => :(emit(:open_paren)),
    :close_paren => :(emit(:close_paren)),
    :str => :(emit(:str)),
    :symbol => :(emit(:symbol)),
    :whitespace => :(emit(:whitespace))
)

# Generate a tokenizing function from the machine.
context = Automa.CodeGenContext()
@eval function tokenize(data::String)
    tokens = Tuple{Symbol,String}[]
    mark = 0
    $(Automa.generate_init_code(context, machine))
    p_end = p_eof = lastindex(data)
    emit(kind) = push!(tokens, (kind, data[mark:p-1]))
    $(Automa.generate_exec_code(context, machine, actions))
    return tokens, cs == 0 ? :ok : cs < 0 ? :error : :incomplete
end

abstract type KObj end

mutable struct KNode <: KObj
    children::Vector{KObj}
end
KNode() = KNode(KObj[])

Base.copy(n::KNode) = KNode(copy(n.children))

function Base.show(io::IO, node::KNode)
    write(io, "(")
    col = 1
    max_col = 60
    for (i,c) in enumerate(node.children)
        i > 1 && write(io, " ")
        sc = split(string(c), "\n")
        if col + length(sc[1]) ≥ max_col
            write(io, "\n  ")
            col = 1
        end
        write(io, sc[1])
        for l in sc[2:end]
            write(io, "\n  ", l)
        end
        length(sc) != 1 && (col += length(sc[end]))
        if col >= max_col && i ∉ [1,length(node.children)]
            write(io, "\n  ")
            col = 1
        end
    end
    write(io, ")")
end

struct KSym <: KObj
    name::String
end
KSym(name::Symbol) = KSym(string(name))
Base.show(io::IO, s::KSym) = write(io, s.name)

struct KStr <: KObj
    str::String
    KStr(str::String) = new(strip(str, '"'))
end
Base.show(io::IO, s::KStr) = write(io, "\"", s.str, "\"")

abstract type KNum <: KObj end

struct KInt{I<:Integer} <: KNum
    i::I
end
Base.show(io::IO, i::KInt) = write(io, string(i.i))
function Base.show(io::IO, i::KInt{<:Unsigned})
    if i.i == 0
        write(io, "0")
    else
        s = repr(i.i)
        write(io, lstrip(uppercase(s[3:end]), '0'))
    end
end

struct KFloat{F<:AbstractFloat} <: KNum
    f::F
end
Base.show(io::IO, f::KFloat) = write(io, string(f.f))

obj(o::Symbol) = KSym(o)
obj(o::String) = KStr(o)
obj(o::I) where {I<:Integer} = KInt(o)
obj(o::F) where {F<:AbstractFloat} = KFloat(o)

function Parse(tokens::Vector{Tuple{Symbol,String}})
    nodes = KNode[]
    cur_node = KNode()
    for (token,value) in tokens
        token == :whitespace && continue
        if token == :open_paren
            push!(nodes, cur_node)
            new_node = KNode()
            push!(cur_node.children, new_node)
            cur_node = new_node
        elseif token == :close_paren
            cur_node = try
                pop!(nodes)
            catch
                error("Unbalanced parentheses")
            end
        else
            node = if token == :symbol
                KSym(value)
            elseif token == :str
                KStr(value)
            elseif token == :dec
                KInt(parse(Int, value))
            elseif token == :hex
                KInt(parse(UInt32, "0x$(value)"))
            elseif token == :float
                KFloat(parse(Float64, value))
            else
                throw(ArgumentError("Unknown token $(token)"))
            end
            push!(cur_node.children, node)
        end
    end
    cur_node.children
end

function Read(s::String)
    tokens,status = tokenize(s)
    status != :ok && error("Tokenization failed: $(status) [$(tokens)]")
    Parse(tokens)
end

Read(io::IO) = Read(read(io, String))

function traverse(visit::Function, tree::KNode)
    stack = KObj[tree]
    while !isempty(stack)
        node = pop!(stack)
        node isa KNode && append!(stack, node.children)
        visit(node)
    end
end

function traverse_nodes(visit::Function, tree::KNode)
    traverse(tree) do obj
        obj isa KNode && visit(obj)
    end
end

function traverse_nodes(visit::Function, tree::KNode, name::String)
    traverse_nodes(tree) do node
        fc = first(node.children)
        fc.name == name && visit(node)
    end
end
