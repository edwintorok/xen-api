let distro = "centos-7"
let ocaml_major, ocaml_minor = 4, 13
let depext_package = "xs-toolstack"
let deps_package = "xapi"
let xs_opam_fork = "edwintorok" (* for now *)

(* packages that introduce more repositories need to run first *)
let repo_packages = ["epel-release"; "centos-release-xen"]
let extra_packages = ["etcd"; "/usr/sbin/ip"; "strace"]

let from_str = Printf.sprintf "docker.io/ocaml/opam:%s-ocaml-%d.%d" distro ocaml_major ocaml_minor
let uid, gid = 1000,1000 (* what the above container contains for the opam user *)

open Bos

type locking = Shared | Locked

let string_of_locking = function Shared -> "shared" | Locked -> "locked"

let run ?(mounts=[]) ?(cache_dirs=[]) cmd =
  let print_mounts () lst =
    String.concat " " (
    List.rev_append mounts (
    lst |> List.map @@ fun (target, uid, gid, locking )->
    Printf.sprintf "--mount=type=cache,target=%s,uid=%d,gid=%d,sharing=%s"
    target uid gid @@ string_of_locking locking))
  in
   Dockerfile.run "%a %s" print_mounts cache_dirs @@ Cmd.to_string cmd

let yum_cache = ("/var/cache/yum", 0, 0, Locked)
let yum_install packages = run ~cache_dirs:[yum_cache] @@ Cmd.(v "sudo" % "yum" % "install" % "-y" %% of_list packages)

let dune_cache = ["/home/opam/.cache/dune", uid, gid, Shared]
let opam_cache =
  (* Printf.sprintf "/home/opam/.opam/%d.%d/.opam-switch/sources"
                  ocaml_major ocaml_minor, uid, gid, Locked)
                  ::*) ("/home/opam/.opam/download-cache", uid, gid, Locked) :: dune_cache

let opam_repository_add repo_url = run Cmd.(v "opam" % "repository" % "add" % "xs-opam" % repo_url)
let opam_repository_rm name = run Cmd.(v "opam" % "repository" % "remove" %% of_list [name])

let opam_depext pkg = run ~cache_dirs:(yum_cache :: opam_cache) @@ Cmd.(v "opam" % "--yes" % "depext" % "--yes" % pkg)
let opam_install args = run ~cache_dirs:opam_cache @@ Cmd.(v "opam" % "install" %% of_list args)

let opam_pin_add pkgs = run
  ~cache_dirs:opam_cache Cmd.(v "opam" % "pin" % "add" % "--yes" % "--no-action" %% of_list pkgs)

let work_dir = "xapi"

let stage_xs_opam_repo = "xs-opam-repository"
let stage_opam_repos = "opam-repos"
let stage_depexts_gen = "depexts-gen"
let stage_depexts_install = "depexts-install"

let xs_opam_src = "xs-opam-master"
let depexts_file = "depexts.pkgs"
let src_dst = "xapi-src"

let containerfile =
  Dockerfile.(
   parser_directive (`Syntax "docker/dockerfile:1") @@@
   [ from ~alias:stage_xs_opam_repo "busybox:latest"
   ; add ~link:true ~chown:(string_of_int uid) ~src:[Printf.sprintf "https://github.com/%s/xs-opam/archive/refs/heads/master.tar.gz" xs_opam_fork] ~dst:"xs-opam.tar.gz" ()
   ; run "tar xzf xs-opam.tar.gz && rm -f xs-opam.tar.gz"

   ; from ~alias:stage_opam_repos from_str
   ; copy ~link:true ~from:stage_xs_opam_repo ~src:[xs_opam_src] ~dst:xs_opam_src ()
   ; opam_repository_add xs_opam_src
   ; opam_repository_rm "default"

   (* avoid having to reinstall packages if the package list doesn't actually change *)
   ; from ~alias:stage_depexts_gen stage_opam_repos
   ; run "opam depext -l %s >%s" depext_package depexts_file
   ; run "ls -la"

   ; from ~alias:stage_depexts_install from_str
   ; copy ~link:true ~from:stage_xs_opam_repo ~src:[xs_opam_src] ~dst:xs_opam_src ()
   ; copy ~link:true ~from:stage_depexts_gen ~src:[Filename.concat "/home/opam" depexts_file] ~dst:depexts_file ()
   ; run "sudo xargs yum install -y <%s" depexts_file
   ; copy ~link:true ~from:stage_opam_repos ~src:["/home/opam/.opam"] ~dst:"/home/opam/.opam" ()

   ; from stage_depexts_install
   ; copy ~link:true ~src:["."] ~dst:src_dst ()
   ; opam_pin_add [src_dst]
   ; opam_install ["--deps-only"; deps_package]
   ]
  )

(*    from from_str
  ; env ["DUNE_CACHE", "enabled"; "DUNE_CACHE_STORAGE_MODE", "copy"]
  ; workdir "%s" work_dir
  ; yum_install repo_packages
  ; yum_install extra_packages
  ; add ~link:true ~src:[Printf.sprintf "https://github.com/%s/xs-opam/archive/refs/heads/master.tar.gz" xs_opam_fork] ~dst:"xs-opam.tar.gz" ()
  ; run "sudo tar xzf xs-opam.tar.gz && sudo rm xs-opam.tar.gz"
  ; opam_repository_add "xs-opam-master"
  ; opam_repository_rm "default" (* TODO: crunch *)
  ; opam_depext depext_package
  ; opam_install ["polly"]
  ; copy ~src:["."]  ~dst:"xapi" ()
  ; opam_pin_add ["xapi/"]
  ; opam_install ["--deps-only"; deps_package]
  ]
  )
*)

let () =
  Dockerfile.string_of_t containerfile |> print_endline

  (* TODO: "stand up a VM or container with dependencies and then just run opam
     pin/etc commands inside of it" then we can use this method with VMs as
     well *)
