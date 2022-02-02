module Opentelemetry : sig
  module Proto : sig
    module Common : sig
      module V1 : sig
        module rec AnyValue : sig
          val name' : unit -> string

          type t =
            [ `Array_value of ArrayValue.t
            | `Bool_value of bool
            | `Bytes_value of bytes
            | `Double_value of float
            | `Int_value of int
            | `Kvlist_value of KeyValueList.t
            | `String_value of string
            | `not_set ]

          val make :
               ?value:
                 [ `Array_value of ArrayValue.t
                 | `Bool_value of bool
                 | `Bytes_value of bytes
                 | `Double_value of float
                 | `Int_value of int
                 | `Kvlist_value of KeyValueList.t
                 | `String_value of string
                 | `not_set ]
            -> unit
            -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and ArrayValue : sig
          val name' : unit -> string

          type t = AnyValue.t list

          val make : ?values:AnyValue.t list -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and KeyValueList : sig
          val name' : unit -> string

          type t = KeyValue.t list

          val make : ?values:KeyValue.t list -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and KeyValue : sig
          val name' : unit -> string

          type t = {key: string; value: AnyValue.t option}

          val make : ?key:string -> ?value:AnyValue.t -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and StringKeyValue : sig
          val name' : unit -> string

          type t = {key: string; value: string}

          val make : ?key:string -> ?value:string -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end

        and InstrumentationLibrary : sig
          val name' : unit -> string

          type t = {name: string; version: string}

          val make : ?name:string -> ?version:string -> unit -> t

          val to_proto : t -> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.t

          val from_proto :
               Ocaml_protoc_plugin.Runtime.Runtime'.Reader.t
            -> (t, [> Ocaml_protoc_plugin.Runtime.Runtime'.Result.error]) result
        end
      end
    end
  end
end
